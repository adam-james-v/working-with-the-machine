(ns naive-sdf
  (:require [solenoid.controls :as c]
            [svg-clj.elements :as el]
            [svg-clj.parametric :as p]
            [svg-clj.tools :as tools]
            [svg-clj.transforms :as tf]
            [svg-clj.utils :as u]))

(defn sphere
  [r]
  (fn [pt]
    (- (u/distance pt [0 0 0]) r)))

(defn translate
  [f pos]
  (fn [pt]
    (f (u/v+ pt pos))))

(defn to-rad
  [deg]
  (/ (* deg Math/PI) 180))

(defn sin-cos-pair [theta]
  [(Math/sin ^double (to-rad theta))
   (Math/cos ^double (to-rad theta))])

(defn rot-pt-2d
  [[x y] theta]
  (let [[s-t c-t] (sin-cos-pair theta)]
    [(- (* x c-t) (* y s-t))
     (+ (* y c-t) (* x s-t))]))

;; this rotates a point around [0,0,0]
(defn rot-pt
  [[x y z] axis theta]
  (cond
    (= axis :x) (into [x] (rot-pt-2d [y z] theta))
    (= axis :y) (apply #(into [] [%2 y %1]) (rot-pt-2d [z x] theta))
    (= axis :z) (into (rot-pt-2d [x y] theta) [z])))

(defn rotate-pt
  [pt [ax ay az]]
  (let [pt (if (< (count pt) 3)
             (conj pt 0)
             pt)]
    (-> pt
        (rot-pt :z az)
        (rot-pt :y ay)
        (rot-pt :x ax))))

(defn rotate-points
  [[ax ay az] pts]
  (mapv #(rotate-pt % [ax ay az]) pts))

(defn rotate
  [f angles]
  (fn [pt]
    (f (rotate-pt pt angles))))

(defn scale
  [f scales]
  (fn [pt]
    (f (u/v* pt scales))))

(defn union [f g]
  (fn [pt]
    (let [a (f pt)
          b (g pt)]
      (min a b))))

(defn difference [f g]
  (fn [pt]
    (let [a (f pt)
          b (* -1 (g pt))]
      (max a b))))

(defn intersection [f g]
  (fn [pt]
    (let [a (f pt)
          b (g pt)]
      (max a b))))

(defn smooth-union-fn
  [k]
  (fn ([a] a)
    ([fa fb]
     (fn [pt]
       (let [a (fa pt)
             b (fb pt)
             h (if (not= k 0)
                 (/ (max (- k (abs (- a b))) 0) k)
                 0)]
         (- (min a b) (* h h h k (/ 1.0 6.0))))))))

(defn clamp
  "clamps a value between lower bound and upper bound"
  [x lb ub]
  (cond
    (< x lb) lb
    (> x ub) ub
    :else x))

(defn polygon
  [pts]
  (fn [[px py :as pt]]
    (let [pts (vec pts)
          n   (count pts)
          d1  (u/dot* (u/v- pt (first pts))
                (u/v- pt (first pts)))]
      (loop [i 0
             j (dec n)
             d d1
             s 1]
        (if (< i n)
          (let [[vix viy :as vi] (get pts i)
                [vjx vjy :as vj] (get pts j)
                [ex ey :as e]    (u/v- vj vi)
                [wx wy :as w]    (u/v- pt vi)
                [bx by :as b]    (u/v-
                                   w
                                   (mapv #(* % (clamp
                                                 (/ (u/dot* w e)
                                                   (u/dot* e e)) 0.0 1.0)) e))
                d                (min d (u/dot* b b))
                c                [(>= py viy)
                                  ( < py vjy)
                                  ( > (* ex wy) (* ey wx))]
                s                (if (= (count (into #{} c)) 1) (* s -1) (* s 1))]
            (recur (inc i) i d s))
          (* s (Math/sqrt d)))))))

(defn stroke
  [f th]
  (let [r (/ th 2.0)]
    (fn [pt]
      (- (abs (f pt)) r))))

(defn sphere
  [r]
  (fn [pt]
    (- (u/distance pt [0 0 0]) r)))

(defn cylinder [r h]
  (fn [pt]
    (let [[x y z] pt]
      (max (- (Math/sqrt (+ (* x x) (* y y))) r)
           (- z (/ h 2)) (- (/ h -2) z)))))

(defn box [l w h]
  (fn [pt]
    (let [[x y z] pt
          [lh wh hh] (map #(/ % 2) [l w h])]
      (max (- x lh) (- (- lh) x)
           (- y wh) (- (- wh) y)
           (- z hh) (- (- hh) z)))))

;; float sdCapsule( vec3 p, vec3 a, vec3 b, float r )
;; {
;;   vec3 pa = p - a, ba = b - a;
;;   float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
;;   return length( pa - ba*h ) - r;
;; }

(defn line
  [[a b] r]
  (fn [pt]
    (let [pa (u/v- pt a)
          ba (u/v- b a)
          h (clamp (/ (u/dot* pa ba) (u/dot* ba ba)) 0 1)]
      (- (u/distance (u/v- pa (u/v* ba [h h h])) [0 0 0]) r))))

(defn extrude
  [frep h]
  (fn [pt]
    (let [d (frep (drop-last pt))
          w (- (Math/abs ^double (- (last pt) (/ h 2))) (/ h 2))]
      (+ (min (max d w) 0)
         (u/distance [0 0] [(max d 0) (max w 0)])))))

(defn revolve
  [f]
  (fn [pt]
    (let [q [(u/distance [0 0] [(first pt) (second pt)])
             (last pt)]]
      (f q))))

(def iso-euler-angles [35.264 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 -90 0])

(defn isometric-xf
  [pts]
  (->> pts
       #_#_(rotate-points origin-angle-adjust-a)
       (rotate-points origin-angle-adjust-b)
       (rotate-points iso-euler-angles)
       (mapv #(into [] (drop-last %)))))

(defn march
  [f [x y]]
  (let [from [x y -200]
        max-steps 150
        min-dist 0.00001]
    (loop [n-steps 0
           total-dist 0]
      (let [pt (u/v+ from [0 0 total-dist])
            d (u/round (f pt) 4)]
        (if (or (> n-steps max-steps)
              (< d min-dist))
          (- 1.0 (/ n-steps max-steps))
          (recur (inc n-steps) (+ total-dist d)))))))

(defn render-pixel
  [pt a]
  (when (> a 0.001)
    (-> (el/rect 1 1)
      (tf/translate [0.5 0.5])
      (tf/translate pt)
      (tf/style {:fill "limegreen"
                 :opacity a}))))

(defn render-frep
  [f [x1 y1] [x2 y2]]
  (let [f        (-> f
                   #_(rotate iso-euler-angles))
        grid-pts (for [y (range y1 (inc y2))
                       x (range x1 (inc x2))]
                   [x y])]
    (->> grid-pts
      (pmap #(render-pixel % (march f %)))
      (remove nil?)
      vec
      (into [:g]))))

(defn render-texel
  [[x y] a]
  (when (> a 0.001)
    (let [#_#_cs "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. "
          cs "@%#o+=-:・ "
          idx (int (* (dec (count cs)) a))]
      [[x y] (get (vec (reverse cs)) idx)])))

(defn print-frep
  [f [x1 y1] [x2 y2]]
  (let [f        (-> f
                   #_(rotate iso-euler-angles)
                   (scale [2.05 1 1]))
        grid-pts (for [y (range y1 (inc y2))
                       x (range x1 (inc x2))]
                   [x y])
        rows (->> grid-pts
               (group-by first)
               vals)
        texels   (->> grid-pts
                   (pmap #(render-texel % (march f %)))
                   (remove nil?)
                   (into {}))]
    (apply str
      (for [row (sort-by second rows)]
        (apply str (conj (mapv #(get texels % " ") row) "\n"))))))

(def blob
  (reduce
    (smooth-union-fn 30)
    [(sphere 30)
     (translate (sphere 20) [0 40 30])
     (translate (sphere 40) [40 50 0])]))

(def blob2
  (reduce
    (smooth-union-fn 40)
    (mapv #(-> (sphere 4)
             (translate (conj % 0)))
      (p/regular-polygon-pts 24 8))))

(def hexblob
  (let [hex (extrude (polygon (p/regular-polygon-pts 27 6)) 60)]
    (reduce
      (smooth-union-fn 60)
      [hex
       #_(-> hex (rotate [0 45 12]) (translate [0 40 30]))
       (-> hex (rotate [90 0 0]) (translate [60 50 -10]))])))

(def leaf-pts
  [[-17 -51]
   [-17 -39]
   [-15 -27]
   [-15 -15]
   [-22 -5]
   [-34 -2]
   [-41 8]
   [-50 20]
   [-50 35]
   [-47 48]
   [-39 63]
   [-24 77]
   [-11 88]
   [-4 104]
   [0 128]
   [15 115]
   [26 91]
   [32 72]
   [37 52]
   [41 35]
   [44 19]
   [39 1]
   [25 -10]
   [12 -14]
   [2 -19]
   [-3 -31]
   [-3 -42]
   [-2 -59]])

(def leaf
  (let [pts (mapv #(u/v* [0.5 0.5] (u/v+ % [2 59])) leaf-pts)
        outline (->> pts
                  (mapv #(conj 0))
                  (partition 2 1)
                  (mapv #(line % 3))
                  #_(reduce (smooth-union-fn 20)))]
    outline
    #_(union
      (-> pts
        polygon
        #_(stroke 1)
        (extrude 1))
      outline)))
