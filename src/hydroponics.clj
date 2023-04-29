(ns hydroponics
  (:require
   [clojure.java.shell :as sh :refer [sh]]
   [scad-clj.model :as sm]
   [scad-clj.scad :refer [write-scad]]
   [clojure.string :as str]))

;; UTILS


;; Shell out to openscad to 'spit' a render of your model to png
(defn png!
  [fname mdl-data]
  (let [scad (write-scad [(sm/fn! 60) mdl-data])]
    (sh "/usr/local/bin/openscad" "/dev/stdin"
        "--imgsize" "500,300"
        "--projection" "orthogonal"
        "--colorscheme" "Tomorrow Night"
        #_#_"--view" "edges"
        #_#_"--camera" "0,0,0,55,0,25,120"
        "-o" fname
        :in scad)))

;; With Cider's 'content types' you can view things in the repl
;; (setq cider-repl-use-content-types t)
;; (setq cider-repl-content-types t)
;; or, M-x cider-repl-toggle-content-types

(defn cider-show
  [mdl-data]
  (let [fname "_tmp.png"]
    (do (png! fname mdl-data)
        (clojure.java.io/file fname))))

(defn place-at
  [pts & block]
  (for [pt pts] (->> block (sm/translate pt))))

(def PI Math/PI)

(defn pipe-xs
  "2D cross-section of a pipe with inner-diameter `id` and wall thickness `t`."
  [id t]
  (let [or (+ (/ id 2) t)]
    (sm/difference
     (sm/circle or)
     (sm/circle (- or t)))))

#_(-> (pipe-xs 4 0.1)
      (sm/extrude-linear {:height 40})
      (cider-show))

(defn main-tube
  [{:keys [tube-p basket-p]}]
  (let [{:keys [l id t hs]} tube-p
        n-holes (int (/ (- l 4) hs))
        hole-offset (/ (- l (* hs (dec n-holes))) 2)
        hd (Math/ceil (* 2 (:tr basket-p)))
        holes (for [x (map #(* hs %) (take n-holes (range)))]
                (->> (sm/cylinder (/ hd 2) id)
                     (sm/translate [(+ x hole-offset) 0 (/ id 2)])))]
    (sm/color
     [0.8 0.8 0.8 1]
     (sm/difference
      (->> (pipe-xs id t)
           (sm/extrude-linear {:height l :center false})
           (sm/rotate [0 (* PI 0.5) 0]))
      holes))))

(defn cap
  [id t h]
  (let [ir (/ id 2)]
    (->> [ [0 0] [ir 0] [ir (- t h)]
           [(+ ir t) (- t h)] [(+ ir t) t] [0 t] ]
         sm/polygon
         (sm/extrude-rotate)
         (#(sm/difference % (sm/cylinder 0.375 (* 2 t))))
         (sm/color [0.65 0.65 0.6 1]))))

(defn caps
  [{:keys [cap-p tube-p]}]
  (let [{:keys [t h]} cap-p
        cap-d (+ (:id tube-p) (* 2 (:t tube-p)))]
    (list
      (->> (cap cap-d t h)
        (sm/rotate [0 (* PI 1.5) 0]))
      (->> (cap cap-d t h)
        (sm/rotate [0 (* PI 0.5) 0])
        (sm/translate [(:l tube-p) 0 0])))))

(defn stand
  [{:keys [w h l]}]
  (sm/color
   [0.6 0.45 0.35 1]
   (sm/difference
    (->> (sm/cube w h l)
         (sm/rotate [(* PI 0.5) 0 0])
         (sm/translate [0 0 (/ h 2)]))
    (->> (sm/cube 3.5 3.5 3.5)
         (sm/rotate [(* PI 0.25) 0 0])
         (sm/translate [0 0 h])))))

(defn stands
  [{:keys [tube-p stand-p cap-p]}]
  (let [offset (* 2 (:h cap-p))]
    (place-at [ [offset 0 0] [(- (:l tube-p) offset) 0 0] ]
              (stand stand-p))))

(defn basket
  [{:keys [tr br h t]}]
  (->> [ [0 0] [br 0]
         [tr (- h t)] [(+ tr (* 1.5 t)) (- h t)]4
         [(+ tr (* 1.5 t)) h] [(- tr (* 1.1 t)) h]
         [(- br (* 1.1 t)) t] [0 t] ]
       sm/polygon
       sm/extrude-rotate
       (sm/translate [0 0 (- t h)])
       (sm/color [0.3 0.3 0.3 1])))

(defn baskets
  [{:keys [tube-p basket-p]}]
  (let [{:keys [l hs]} tube-p
        n (int (/ (- l 4) hs))
        offset (/ (- l (* hs (dec n))) 2)
        voffset (+ (/ (:id tube-p) 2) (:t tube-p))]
    (place-at
     (map #(vector (+ offset (* hs %)) 0 voffset) (take n (range)))
     (basket basket-p))))

(defn assembly
  [{:keys [tube-p stand-p] :as p}]
  (let [d (+ (:id tube-p) (* 2 (:t tube-p)))
        h (+ (/ d 2) (- (:h stand-p) (* 0.275 d)))]
    (list
     (sm/translate [(/ (:l tube-p) -2.0) 0 0]
                   (map #(sm/translate [0 0 h] %)
                        (concat [(main-tube p)]
                                (caps p)
                                (baskets p)))
                   (stands p)))))

(def parameters
  {:tube-p   {:l  42
              :id 5
              :t  0.1875
              :hs 4.75}
   :cap-p    {:t  0.2
              :h  2}
   :basket-p {:tr 1.45
              :br 1.1
              :h  2.25
              :t  0.1}
   :stand-p  {:w  1.25
              :h  3.5
              :l  12}})

(spit "design.scad" (write-scad (assembly parameters)))

(def this-directory (->> (sh "pwd") :out str/trim))

(defn scad->csg
  [scad-str]
  (-> (sh "/usr/local/bin/openscad" "/dev/stdin"
        "--export-format" "csg"
        "-o" "-" :in scad-str)
    :out
    str/trim))

(defn exporter-script [ipath opath]
  (format "
import FreeCAD
import importCSG
import Import

App.newDocument(\"a\")
doc = FreeCAD.getDocument(\"a\")
importCSG.insert(u\"%s\", \"a\")
__objs__ = doc.RootObjects
Import.export(__objs__, u\"%s\")
del __objs__" ipath opath))

(defn step!
  [f mdl-data]
  (let [path this-directory
        csg (-> mdl-data write-scad scad->csg)
        exporter-script (exporter-script (str path "/scadout.csg") (str path "/" f))]
    (spit "scadout.csg" csg)
    (spit "fcscript.py" exporter-script)
    (sh "/Applications/FreeCAD.app/Contents/Resources/bin/freecadcmd" "fcscript.py")
    (sh "rm" "-rf"
        "fcscript.py"
        "parsetab.py"
        "scadout.csg"
        "__pycache__")))

(comment
  ;; see a render in the REPL
  (cider-show (assembly parameters))

  ;; Create a STEP file of the assembly
  (step! "out.step" (assembly parameters))
  (sh "open" "out.step")

  )
