(ns x3d
  (:require [solenoid.server :as ss]
            [solenoid.components :as components]
            [solenoid.controls :as c]
            [clojure.java.io :as io]))

(require 'x3d :reload-all)
(reset! c/registry {})

(ss/serve! {:port 9876
            :head-entries [#_[:script (slurp (io/resource "x_ite.min.js"))]
                           [:script {:src "https://create3000.github.io/code/x_ite/latest/x_ite.min.js"}]]})

(c/letview {:width 2 :height 2 :x  4000 :y  7000} [:div])

(defmethod components/render-control-block-result :x3d
  [{:keys [id state]}]
  (let [result @state]
    [:Group.text-center
     (merge
       {:id          (str (name id) "-result")
        :class       ["control-block-result"]
        :hx-swap-oob "morphdom"})
     (or result "no result")]))

(defn x3d-result-wrap-fn
  [result]
  [:x3d-canvas
   {:style {:margin "0 auto"
            :width "100%" #_"350px"
            :height "400px"}}
   [:X3D {:profile "Full"
          :version "8.5.2"}
    [:Scene
     [:Background {:skyColor    "0.2 0.2 0.2"}]
     [:ViewpointGroup
      [:Viewpoint {:description "Perspective"
                   :position    "500 500 500"
                   :viewAll     "true"}]]
     result]]])

(defmethod components/render-control-block :x3d
  [control-block]
  (components/render-control-block*
    control-block
    (x3d-result-wrap-fn
      (components/render-control-block-result control-block))))

(defn round
  "Rounds a non-integer number `num` to `places` decimal places."
  ([num] (round num 5))
  ([num places]
   (if places
     (let [d (bigdec (Math/pow 10 places))]
       (double (/ (Math/round (* (double num) d)) d)))
     num)))

(defn distance
  "Computes the distance between two points `a` and `b`."
  [a b]
  (let [v (mapv - b a)
        v2 (reduce + (mapv * v v))]
    (round (Math/sqrt ^double v2))))

(defn regular-polygon-pts
  "Return a list of points making up a polygon with distance to the points `r` and `n` edges."
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (map #(vector (round (* r (Math/cos (* % angle))) 5)
                  (round (* r (Math/sin (* % angle))) 5))
         (range n))))

(defn average
  "Compute the average of `numbers`."
  [& numbers]
  (let [n (count numbers)]
    (round (/ (apply + numbers) n))))

(defn centroid-of-pts
  "Calculates the arithmetic mean position of the given `pts`."
  [pts]
  (let [ndim (count (first (sort-by count pts)))
        splits (for [axis (range 0 ndim)]
                 (map #(nth % axis) pts))]
    (mapv #(apply average %) splits)))

(defn- remap-within
  "Shift the parameter range of `f` from 0 to 1 to `start` to `end`."
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

(defn line
  "Create a parametric function representing a straight line.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = `a` and t 1 = `b`."
  [a b]
  (fn
    ([] {:input [a b]
         :origin (centroid-of-pts [a b])
         :vertex-params [0 1]
         :length (distance a b)})
    ([t]
     (cond
       (= (float t) 0.0) a
       (= (float t) 1.0) b
       :else
       (mapv + a (mapv * (mapv - b a) (repeat t)))))))

(defn circle
  "Create a parametric function representing a circle with radius `r` centered at the origin, or circumscribing points `a`, `b`, and `c`, as long as the three points are not colinear.
  The returned function takes a parameter `t` between 0 and 1, where t 0 and 1 = [r 0] or centroid + calcuated radius."
  [r]
  (fn
    ([] {:input [r]
         :origin [0 0]
         :vertex-params [0]
         :length (* Math/PI 2 r)})
    ([t]
     (let [t (* 2 Math/PI t)
           x (* r (Math/cos t))
           y (* r (Math/sin t))]
       [x y]))))

(defn polygon
  "Create a parametric function representing a polygon with straight segments defined by `pts`.
  The returned function takes a parameter `t` between 0 and 1, where t 0 and 1 = (first `pts`)."
  [pts]
  (let [pts (concat (vec pts) [(first pts)])
        lines (map (partial apply line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))]
    (fn
      ([] {:input [pts]
           :origin (centroid-of-pts pts)
           :vertex-params (concat [0] (mapv second intervals))
           :length (reduce + (map #(:length (%)) lines))})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) lines intervals))))))))

(defn line3d
  [pts]
  (let [pts       (mapv (fn [[x y z]]
                          (if (not z)
                            [x y 0]
                            [x y z])) pts)
        stringify (fn [pts]
                    (->> pts
                         (interpose [","])
                         (apply concat)
                         (map str)
                         (interpose " ")
                         (apply str)))
        profile   (->> (regular-polygon-pts 0.75 16) (mapv (fn [[x y]] [x 0 y])))
        profile   (concat profile [(first profile)])]
    [:Shape
     [:Appearance
      [:Material {:diffuseColor  "2,2,2"
                  :emissiveColor "2,2,2"
                  :creaseAngle   "2"}]]
     [:Extrusion {:ccw          "true"
                  :solid        "true"
                  :convex       "true"
                  :beginCap     "true"
                  :endCap       "true"
                  :crossSection (stringify profile)
                  :spine        (stringify pts)}]]))


(defn fastline
  "Create a parametric function representing a straight line, with no checks and slightly faster implementation meant primarily for use in the bezier implementation.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = `a` and t 1 = `b`."
  [a b]
  (fn [t]
    (mapv (fn [da db] (+ da (* (- db da) t))) a b)))

(defn arc-length
  "Calculate the arc length of `curve`, being exact where possible and estimating otherwise.
  For example, bezier curves are estimated, but circles and arcs have exact results (barring rounding)."
  ([curve] (arc-length curve 0 1))
  ([curve t] (arc-length curve 0 t))
  ([curve ta tb]
   (let [seg   200 ;; keep this number kinda low to keep speeds up. It's not a great solution...
         start (/ (* ta seg) seg)
         end   (/ (inc (* tb seg)) seg)
         ts    (range start end (/ 1 seg))]
     ;; try a transducer approach. Probably room for improvements in this whole thing, honestly :)
     (transduce (comp
                  (map curve)
                  (partition-all 2)
                  (remove #(= 1 (count %)))
                  (map #(apply distance %)))
                +
                (interleave ts (rest ts)))
     #_(->> (range start end (/ 1 seg))
          (map curve)
          (partition 2 1)
          (map #(apply distance %))
          (reduce +)
          (#(round % 5))))))

(defn- quadratic-bezier
  [a b c]
  (fn [t]
    (let [l1 (fastline a b)
          l2 (fastline b c)
          l3 (fastline (l1 t) (l2 t))]
      (l3 t))))

(defn- bezier*
  [pts]
  (if (= 3 (count pts))
    (apply quadratic-bezier pts)
    (let [lines (map #(apply fastline %) (partition 2 1 pts))]
      (fn
        [t]
        (let [npts (map #(% t) lines)]
          ((bezier* npts) t))))))

(defn bezier
  "Create a parametric function representing a bezier curve with control points `pts`, as long as there are at least 3 points.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = (first `pts`) and t 1 = (last `pts`)."
  [pts]
  (when (> (count pts) 2)
    (let [curve (bezier* pts)
          length (arc-length curve)]
      (fn
        ([] {:fn `bezier
             :input [pts]
             :origin (centroid-of-pts pts)
             :vertex-params [0 1]
             :length length})
        ([t] (curve t))))))

(def pts-a
  (c/letcontrols {:width 270 :height 270 :x 10 :y 10}
    [r {:display-name "R" :type :slider :value 10 :min 1 :max 200 :step 0.1}
     n {:display-name "N" :type :num :value 8 :min 3 :max 200 :step 1}]
    (let [pts (regular-polygon-pts r n)
          [w h] [150 150]
          pts-str (->> pts
                       (map (fn [[x y]] (str x "," y)))
                       (interpose [" "])
                       (apply concat)
                       (apply str))]
      (with-meta
       [:svg {:viewBox (format "%s %s %s %s" (int (* w -0.5)) (int (* h -0.5)) w h)
              :width w
              :height h}
        [:polygon {:points pts-str
                   :fill "none"
                   :stroke "cyan"
                   :stroke-width 2}]]
       {:pts pts}))))

(defn stringify
  [pts]
  (->> pts
       (interpose [","])
       (apply concat)
       (map str)
       (interpose " ")
       (apply str)))

(def control-circle-params
  (c/letcontrols {:width 390 :height 260 :x 10 :y 290}
    [r1 {:display-name "R1" :type :slider :value 30 :min 0 :max 500 :step 0.01}
     r2 {:display-name "R2" :type :slider :value 30 :min 0 :max 500 :step 0.01}
     r3 {:display-name "R3" :type :slider :value 30 :min 0 :max 500 :step 0.01}
     r4 {:display-name "R4" :type :slider :value 39 :min 0 :max 500 :step 0.01}
     h1 {:display-name "H1" :type :slider :value 0 :min 0 :max 500 :step 0.01}
     h2 {:display-name "H2" :type :slider :value 50 :min 0 :max 500 :step 0.01}
     h3 {:display-name "H3" :type :slider :value 100 :min 0 :max 500 :step 0.01}
     h4 {:display-name "H4" :type :slider :value 200 :min 0 :max 500 :step 0.01}]
    (with-meta
     [:span {:display "none"} ""]
     {:data {:r1 r1
             :r2 r2
             :r3 r3
             :r4 r4
             :h1 h1
             :h2 h2
             :h3 h3
             :h4 h4}})))

;; Then, you can use those values in other blocks:

(def control-point-params
  (c/letcontrols {:width 370 :height 580 :x 410 :y 10}
    [t1 {:display-name "T1" :type :slider :value 0.115 :min 0 :max 1 :step 0.001}
     t2 {:display-name "T2" :type :slider :value 0.252 :min 0 :max 1 :step 0.001}
     t3 {:display-name "T3" :type :slider :value 0.735 :min 0 :max 1 :step 0.001}
     t4 {:display-name "T4" :type :slider :value 0.130 :min 0 :max 1 :step 0.001}]
    (let [{:keys [r1 r2 r3 r4 h1 h2 h3 h4]} (-> @control-circle-params meta :data)
          add-h (fn [c h]
                  (fn [t]
                    (let [[x y] (c t)]
                      [x h y])))
          [c1 c2 c3 c4 :as cs] (map add-h (map circle [r1 r2 r3 r4]) [h1 h2 h3 h4])
          f (fn [c]
              (->> (map c (range 0 1.05 0.05))
                   line3d))
          ptfn (fn [[x y z]]
                 [:Transform {:translation (format "%s, %s, %s" x y z)}
                  [:Shape
                   [:Appearance
                    [:Material {:diffuseColor "red"
                                :emissiveColor "red"}]]
                   [:Sphere {:radius 4}]]])
          control-pts (map (fn [c t] (c t)) [c1 c2 c3 c4] [t1 t2 t3 t4])]
      (with-meta
       (into [:Group]
             (concat
               (map f cs)
               (map ptfn control-pts)
               [(line3d (map (bezier control-pts) (range 0 1.05 0.05)))]))
       {:control-block-type :x3d
        :result-type        :x3d
        :control-pts control-pts }))))

(defn distribution-fn
  [st spread]
  (fn [t]
    (Math/pow 2
              (* (/ -105 spread)
                 (Math/pow (- t 0.0 st) 2)))))

(defn spread-scale
  [t t2 scale spread ts]
  (let [spread-scales (map (distribution-fn t spread) ts)]
    (->> (map #(+ t2 (* scale %)) spread-scales)
         (mapv (fn [v] (mapv double [v v]))))))

(def curve
  (c/letcontrols {:width 500 :height 600 :x 10 :y 610}
    [steps {:display-name "STEPS" :type :num :value 30 :min 10 :max 200 :step 1}
     t1 {:display-name "T1" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
     sc {:display-name "SCALE" :type :slider :value 0.001 :min 1 :max 10 :step 0.0001}
     sp {:display-name "SPREAD" :type :slider :value 1 :min 0.001 :max 10 :step 0.0001}
     t4 {:display-name "T4" :type :slider :value 1 :min 0 :max 1 :step 0.001}]
    (let [c       (bezier (-> @control-point-params meta :control-pts)) ;; parametric curve from the bezier points
          ts      (range 0 1 (double (/ 1 (or steps 10))))
          ts      (if (< (apply max ts) 1)
                    (conj (vec ts) 1)
                    ts)
          pts     (mapv (fn [[x y z]]
                          (if (not z)
                            [x y 0]
                            [x y z])) (map c ts))
          profile-pts (->> @pts-a meta :pts reverse)
          profile (concat profile-pts [(first profile-pts)])
          scales  (spread-scale t1 t4 sc sp ts)]
      (with-meta
       [:Shape
        [:Appearance
         [:Material {:diffuseColor      "salmon"
                     :creaseAngle       "0"}]]
        [:Extrusion {:ccw          "true"
                     :solid        "true"
                     :convex       "true"
                     :beginCap     "true"
                     :endCap       "true"
                     :scale        (stringify scales)
                     :crossSection (stringify profile)
                     :spine        (stringify pts)}]]
        {:control-block-type :x3d
         :result-type        :x3d}))))
