(ns physdrone.core
  (:use [overtone.live])
  (:require [quil.core :as q]))

(definst ping [freq 440 dur 3 vel 1]
  (let [env (env-gen (envelope [1 0.0001] [dur] :exponential) :action FREE)]
    (* (sin-osc freq)
       env
       vel)))

(defn grav-v [{[px py] :pos [vx vy] :vel} [gx gy] g]
  (let [d (+ (* (- gx px) (- gx px)) (* (- gy py) (- gy py)))
        f (/ g d)
        alpha (Math/atan2 (- py gy) (- px gy))
        fx (* f (Math/cos alpha))
        fy (* f (Math/sin alpha))]
    [fx fy]))

(defn next-point [pt]

  (let [[vgx vgy] (grav-v pt [0 0] 1800)
;        [rgx rgy] (grav-v pt [0 0] -500)
        {[px py] :pos [vx vy] :vel} pt

        damp 0.001
        vx (- vx (* vx vx damp))
        vy (- vy (* vy vy damp))

        vx (- vx vgx)
        vy (- vy vgy)

        vx (if (< 300 (Math/abs (+ px vx))) (- vx) vx)
        vy (if (< 300 (Math/abs (+ py vy))) (- vy) vy)]

    (if (neg? (* py (+ vy py)))
      (do
        (ping (* 4 px) 3 (* 0.1 vy))))

    {:pos [(+ vx px) (+ vy py)]
     :vel [vx vy]}))

(defn setup []
  (q/background 20)
  (q/smooth)
  (q/set-state! :points (map
                         (fn [v] (atom {:pos [0.01 (+ 250 (* 2 v))]
                                       :vel [(+ 1.5 (* 0.1 v)) 0]}))
                         (range 10))))

(defn draw []
  (q/background 20)
  (q/color 255)
  (q/line 0 300 600 300)

  (q/ellipse 300 300 20 20)

  (let [pts (q/state :points)]
    (dorun (for [pt pts]
             (let [{[px py] :pos} @pt]
               (q/ellipse (+ 300 px) (+ 300 py) 10 10)
               (reset! pt (next-point @pt)))))))





(q/defsketch sketch-name
  :title "physdrone"
  :setup setup
  :draw draw
  :size [600 600]
  :renderer :opengl
  :target :perm-frame)
















(comment (defn -main
           "I don't do a whole lot."
           [& args]
           (ping)))
