(ns tileworks
  (:require [hiccup.core :as h]
            [nextjournal.markdown :as md]
            [nextjournal.markdown.transform :as md.transform]
            [oatmilk.polygon-intersection :as pi]
            [solenoid.components :as components]
            [solenoid.controls :as c]
            [solenoid.server :as ss]
            [svg-clj.composites :as composites :refer [svg]]
            [svg-clj.elements :as el]
            [svg-clj.layout :as l]
            [svg-clj.parametric :as p]
            [svg-clj.path :as path]
            [svg-clj.tools :as tools]
            [svg-clj.transforms :as tf]
            [svg-clj.utils :as u]))

(require 'tileworks :reload-all)
(reset! c/registry {})
(ss/serve!)

(defmethod components/render-control-block-result :note
  [{:keys [id state]}]
  (let [result @state]
    [:div
     (merge
       {:id          (str (name id) "-result")
        :class       ["control-block-result"]
        :hx-swap-oob "morphdom"
        :style {:font-family "Charter"
                :font-size "1.25rem"
                :line-height "1.25em"
                :color "black"
                :padding "10px"}})
     result]))

(defn letnote
  [& md-strings]
  (let [parsed-markdown (md/->hiccup (apply str (interpose "\n" md-strings)))
        [k _ maybe-title] (second parsed-markdown)
        title (when (= k :h1) maybe-title)]
    (c/letview {:title title
                :width 380
                :height 300}
      (with-meta
        (cond-> parsed-markdown
          title (assoc 1 nil))
       {:result-type :note}))))

(letnote
 "# Tileworks Note"
 "### Tileworks"
 "Tileworks is a series of quilt patterns designed partly in Clojure."
 "I've worked on these designs with my mom. ❤️")



;; utils

(def epsilon 0.000001)

(defn pt-in
  [[_ {xl :x yl :y w :width h :height}]]
  (let [xr (+ xl w)
        yr (+ yl h)]
    (fn [[x y]]
      (and (< xl x xr)
           (< yl y yr)))))

(defn round-pts
  ([pts] (round-pts pts 0))
  ([pts places]
   (mapv (fn [pt] (mapv #(svg-clj.utils/round % 0) pt)) pts)))

(defn shift-one
  [[a & pts]]
  (vec (concat pts [a])))

(defn shift-n
  [pts n]
  (if (= n 0)
    pts
    (last (take (inc n) (iterate shift-one pts)))))

(defn circle-circle-intersection
  [[_ {r1 :r x1 :cx y1 :cy :as c1}]
   [_ {r2 :r x2 :cx y2 :cy :as c2}]]
  (let [p1 [x1 y1]
        p2 [x2 y2]
        d (if (and (< (Math/abs (- r1 r2)) epsilon)
                   (< (Math/abs (- x1 x2)) epsilon)
                   (< (Math/abs (- y1 y2)) epsilon))
            0
            (u/distance p1 p2))
        dr (abs (- r1 r2))]
    (cond
      ;; no solutions
      (>= d (+ r1 r2)) []
      (<= d dr) []
      ;; infinite number of solutions
      (and (< d epsilon) (< dr 0)) []

      :else
      (let [;; a = (r02 - r12 + d2 ) / (2 d)
            a (/ (+ (- (* r1 r1) (* r2 r2)) (* d d)) (* 2 d))
            ;;  h2 = r02 - a2
            h (Math/sqrt (- (* r1 r1) (* a a)))
            ;; P2 = P0 + a ( P1 - P0 ) / d
            [xm ym] (u/v+ p1 (u/v* [(/ a d) (/ a d)] (u/v- p2 p1)))
            ;; x3 = x2 +- h ( y1 - y0 ) / d
            ;; y3 = y2 -+ h ( x1 - x0 ) / d
            x (+ xm (/ (* h (- y2 y1)) d))
            y (- ym (/ (* h (- x2 x1)) d))]
        [[x y]
         [x (- y)]]))))

(defn remove-close-pts
  [pts]
  (let [eps 0.25
        pairs (remove
                (fn [[a b]]
                  (< (u/distance-squared a b) (* eps eps)))
                (partition 2 1 (concat pts [(first pts)])))]
    (if (= (count pairs) (count pts))
      pts
      (recur (mapv first pairs)))))

;; Visual Debug REPL tools

(defn show-ends
  [pts]
  (let [[s e] ((juxt first last) pts)]
    (tools/cider-show
     (el/g
      (-> pts
          el/polyline
          (tf/style {:stroke "slategray"
                     :stroke-width "2px"
                     :fill "none"
                     :opacity 0.7}))
      (-> (el/circle 3)
          (tf/translate s)
          (tf/style {:fill "green"}))
      (-> (el/circle 3)
          (tf/translate e)
          (tf/style {:fill "red"}))))))

(defn show-centroid
  [pts]
  (let [origin [0 0]
        centroid (u/centroid-of-pts pts)]
    (tools/cider-show
     (el/g
      (-> pts
          el/polygon
          (tf/style {:stroke "slategray"
                     :stroke-width "2px"
                     :fill "white"
                     :opacity 0.5}))
      (-> (el/circle 3)
          (tf/translate origin)
          (tf/style {:fill "green"}))
      (-> (el/circle 3)
          (tf/translate centroid)
          (tf/style {:fill "red"}))))))

(defn show-shape
  [pts]
  (-> pts
      el/polygon
      (tf/style {:stroke "white"
                 :stroke-width "2px"
                 :fill "#b48ead"
                 :opacity 0.7})
      tools/cider-show))

;; Tileworks

(def tileworks-controls
  ;; define some control elements
  (c/letcontrols
   {:title "Tileworks Controls"
    :width 525
    :height 525}
    [ir  {:display-name "Inner R" :value 11 :min 2 :max 100 :step 0.001 :type :slider}
     or  {:display-name "Outer R" :value 49 :min 2 :max 100 :step 0.001 :type :slider}
     tr  {:display-name "Tile Edge R" :value 42 :min 2 :max 100 :type :slider}
     pr  {:display-name "Pattern R" :value 31 :min 2 :max 100 :type :slider}
     pn  {:display-name "Pattern N" :value 12 :type :num}]
    (with-meta
     [:div {:style {:display "none"}}]
     {:sc     5
      :ir     ir
      :or     or
      :sct [0 0]
      :tr     tr
      :pr     pr
      :pn     pn})))

(def tileworks-selector
  (c/letcontrols
   {:title  "SELECTOR"
    :width  800
    :height 800}
   [window {:display-name ""
            :value        [[72 -155]
                           [99 -67]
                           [62 21]
                           [-58 21]
                           [-104 -106.75]
                           [-101 -167.75]
                           [0 -247.75]
                           [48 -210.75]]
            :width        700 :height 700 :type :point :n-pts 8}]
   (with-meta
    [:div {:style {:display "none"}}]
    {:inclusion window})))

(def tileworks-excluder
  (c/letcontrols
   {:title  "EXCLUDER"
    :width  800
    :height 800}
   [window {:display-name ""
            :value        [[43 -163]
                           [69 -79]
                           [55 6]
                           [-19 -69]
                           [24.5 -66.75]
                           [-54.5 8.25]
                           [-66.5 -103.75]
                           [-46.5 -156.75]]
            :width        700 :height 700 :type :point :n-pts 8}]
   (with-meta
    [:div {:style {:display "none"}}]
    {:exclusion window})))

(def tileworks
  ;; define a visual aid
  (c/letview
   {:title "Tileworks Visual"
    :width 810
    :height 810}
    ;; bind some vals to a scaled multiple
    (let [{:keys [sc ir or sct tr pr pn]} (-> @tileworks-controls meta)
          {:keys [inclusion]} (-> @tileworks-selector meta)
          {:keys [exclusion]} (-> @tileworks-excluder meta)

          ir              (* sc ir)
          or              (* sc or)
          tr              (* sc tr)
          pr              (* sc pr)
          sct              [0 0]
          pn              pn
          base-style      (fn [el]
                            (-> el
                              (tf/style {:fill         "none"
                                         :stroke       "black"
                                         :stroke-width 1
                                         :opacity      0.6})))
          pattern-circles (drop 2 (l/pattern-on-pts
                                    (-> (el/circle tr) base-style)
                                    (p/regular-polygon-pts pr pn)))
          inner-circle    (-> (el/circle ir) base-style (tf/style {:stroke "red"}))
          outer-circle    (-> (el/circle or) base-style (tf/style {:stroke "red"}))
          inclusion          (mapv (fn [pt] (mapv + pt sct)) inclusion)
          exclusion          (mapv (fn [pt] (mapv + pt sct)) exclusion)
          all-circles     (concat [inner-circle outer-circle] pattern-circles)
          intersections   (->> all-circles
                            (mapcat (fn [c] (mapcat #(circle-circle-intersection % c) all-circles)))
                            (remove #(< (u/distance [0 0] %) ir))
                            (remove #(> (u/distance [0 0] %) or))
                            #_(filter (pt-in window))
                            pi/->pts
                            (filter (pi/pt-inside? (pi/->poly inclusion)))
                            (remove (pi/pt-inside? (pi/->poly exclusion)))
                            pi/sort-pts-ccw
                            (mapv (fn [{:keys [x y]}] [x y]))
                            remove-close-pts)]
      (-> (el/g
            (el/g pattern-circles)
            inner-circle
            outer-circle
            (-> (el/polygon inclusion) (tf/style {:fill "none" :stroke "indigo" :stroke-width "1.5px"}))
            (when (seq intersections)
              (el/g
                (concat
                  (mapv #(-> (el/circle 2) (tf/translate %) (tf/style {:fill "cyan"}))
                    intersections)
                  [(-> (el/polygon intersections) (tf/style {:fill "green" :stroke "lightgreen" :opacity 0.5}))])))
            (-> (el/polygon exclusion) (tf/style {:fill "none" :stroke "red" :stroke-width "1.5px"})))
        svg
        ;; pass intersections out via meta
        (with-meta {:intersections intersections})))))

;; grab the intersection points for use in polygons later
(def my-points (-> tileworks deref meta :intersections))
(def raw-base-shape-a
  [[73.0 -42.0]
   [67.0 -18.0]
   [61.0 0.0]
   [54.0 15.0]
   [0.0 -61.0]
   [-54.0 15.0]
   [-61.0 0.0]
   [-67.0 -18.0]
   [-73.0 -42.0]
   [-76.0 -76.0]
   [-71.0 -123.0]
   [-48.0 -181.0]
   [0.0 -239.0]
   [48.0 -181.0]
   [71.0 -123.0]
   [76.0 -76.0]])

(def raw-base-shape-b
  [[49.0 -49.0]
   [53.0 -30.0]
   [54.0 -15.0]
   [54.0 15.0]
   [-54.0 15.0]
   [-54.0 -15.0]
   [-53.0 -30.0]
   [-49.0 -49.0]
   [-42.0 -73.0]
   [-28.0 -103.0]
   [0.0 -142.0]
   [28.0 -103.0]
   [42.0 -73.0]])

(def raw-base-cut-pts
  [[54.0 -15.0]
   [54.0 15.0]
   [-54.0 15.0]
   [-54.0 -15.0]
   [-40.0 -40.0]
   [-15.0 -54.0]
   [15.0 -54.0]
   [40.0 -40.0]])

(def raw-base-shape-c [[48.0 -181.0] [0.0 -142.0] [-48.0 -181.0] [0.0 -239.0]])
(def raw-base-shape-d [[-28.0 -103.0] [-71.0 -123.0] [-48.0 -181.0] [0.0 -142.0]])
(def raw-base-shape-f [[-42.0 -73.0] [-76.0 -76.0] [-71.0 -123.0] [-28.0 -103.0]])

(def base-shape-a
  (let [chain-a  (-> raw-base-shape-a
                     (shift-n 4)
                     rest
                     reverse)
        chain-b  (-> raw-base-cut-pts
                     (shift-n 2)
                     rest
                     drop-last)
        combined (concat chain-a chain-b)
        ctr      (u/centroid-of-pts combined)
        manual-offset [0 8.238100000000003]]
    (->> combined
         (mapv #(u/v- % (u/v+ ctr manual-offset))))))

(def base-shape-b
  (let [chain-a  (-> raw-base-shape-b
                     (shift-n 4)
                     rest
                     drop-last
                     reverse)
        chain-b  (-> raw-base-cut-pts
                     (shift-n 2)
                     rest
                     drop-last)
        combined (concat chain-a chain-b)
        ctr      (u/centroid-of-pts combined)
        manual-offset [0 0]]
    (->> combined
         (mapv #(u/v- % (u/v+ ctr manual-offset))))))

(def base-shape-c
  (let [manual-offset [0 -53.7619]]
    (->> raw-base-shape-c
         (mapv #(u/v- % manual-offset)))))

(def base-shape-d
  (let [manual-offset [0 -53.7619]]
    (->> raw-base-shape-d
         (mapv #(u/v- % manual-offset)))))

(def base-shape-e
  (let [manual-offset [0 -53.7619]]
    (->> raw-base-shape-d
         (mapv (fn [[x y]] [ (- x) y]))
         (mapv #(u/v- % manual-offset)))))

(def base-shape-f
  (let [manual-offset [0 -53.7619]]
    (->> raw-base-shape-f
         (mapv #(u/v- % manual-offset)))))

(def base-shape-g
  (let [manual-offset [0 -53.7619]]
    (->> raw-base-shape-f
         (mapv (fn [[x y]] [ (- x) y]))
         (mapv #(u/v- % manual-offset)))))

(def tileworks-layout
  (c/letcontrols
   [pr  {:display-name "Pattern R" :value 55.625 :min 10 :max 400 :step 0.001 :type :slider}
    rot  {:display-name "Angle Offset" :value 30 :type :num}
    rotb  {:display-name "B Piece Offset" :value 60 :type :num}
    pn  {:display-name "Pattern N" :value 3 :min 1 :type :num}]
   (let [groups        [{:shapes [base-shape-a
                                  base-shape-c
                                  base-shape-d
                                  base-shape-e
                                  base-shape-f
                                  base-shape-g]
                         :rotation 0}
                        {:shapes [base-shape-b]
                         :rotation rotb}]
         piece-cols    [{:fill "#be8ead" :stroke "gray" :opacity 0.4}
                        {:fill "#a3be8c" :stroke "gray" :opacity 0.4}
                        {:fill "#ebcb8b" :stroke "gray" :opacity 0.4}
                        {:fill "#d08770" :stroke "gray" :opacity 0.4}
                        {:fill "#bf616a" :stroke "gray" :opacity 0.4}
                        {:fill "#8fbcbb" :stroke "gray" :opacity 0.4}]
         pattern-angle (/ 360 pn)]
     (-> (el/g
          (-> (el/rect 300 300) (tf/style {:fill "none"}))
          (-> (el/circle 4) (tf/translate [0 0]) (tf/style {:fill "red" :stroke "red"}))
          (-> (el/circle pr) (tf/style {:fill "none" :stroke "red"}))
          (-> (el/g (for [{:keys [shapes rotation]} groups]
                      (let [base (el/g
                                  (map (fn [pts style]
                                         (when (seq pts)
                                           (-> (el/polygon pts)
                                               (tf/style style)))) shapes (cycle piece-cols)))]
                        (-> (el/g
                             (map (fn [pos i]
                                    (let [a (* i pattern-angle)]
                                      (-> base
                                          (tf/rotate 90)
                                          (tf/rotate a)
                                          (tf/translate pos))))
                                  (p/regular-polygon-pts pr pn)
                                  (range pn)))
                            (tf/rotate rotation)))))
              (tf/rotate rot)))
         svg))))

  ;; grab the svg

#_(-> tileworks-layout deref tools/cider-show)
