(ns physdrone.core
  (:use [overtone.live])
  (:require [quil.core :as q]))

(definst ping [freq 440 dur 3 vel 1 pan 0.5]
  (let [env (env-gen (envelope [1 0.0001] [dur] :exponential) :action FREE)
        sig (* (sin-osc freq) env vel)]
    [(* (- 1 pan) sig) (* pan sig)]))

(defn clip-in [m v]
  (let [pm m
        nm (- m)]
    (cond (> v pm) pm
          (< v nm) nm
          :otherwise v)))

(defn grav-v [{[px py] :pos [vx vy] :vel} [gx gy] g]
  (let [d (+ (* (- gx px) (- gx px)) (* (- gy py) (- gy py)))
        f (/ g d)
        alpha (Math/atan2 (- py gy) (- px gy))
        max-f 10
        fx (clip-in max-f (* f (Math/cos alpha)))
        fy (clip-in max-f (* f (Math/sin alpha)))]
    [fx fy]))

(defn add-ring [x _]
  (swap! (q/state :rings) conj (atom [x 0 0])))

(defn next-point [pt]

  (let [[vgx vgy] (grav-v pt [0 0] 1800)

        {[px py] :pos [vx vy] :vel} pt

;        damp 0.001
;        vx (- vx (* vx vx damp))
;        vy (- vy (* vy vy damp))

        vx (- vx vgx)
        vy (- vy vgy)

        vx (if (< 290 (Math/abs (+ px vx))) (- vx) vx)
        vy (if (< 290 (Math/abs (+ py vy))) (- vy) vy)]

    (if (neg? (* py (+ vy py)))
      (do
        (add-ring px py)
        (ping (* 4 px) 3 (* 0.1 vy) (/ px 300))))

    {:pos [(+ vx px) (+ vy py)]
     :vel [vx vy]}))

(defn setup []
  (q/background 20)
  (q/smooth)
  (q/set-state! :points (map
                         (fn [v] (atom {:pos [0.01 (+ 250 (* 2 v))]
                                       :vel [(+ 1.5 (* 0.1 v)) 0]}))
                         (range 10))
                :rings (atom [])))

(defn draw []
  (q/background 20)

  (q/stroke 0 0 0)
  (q/line 0 300 600 300)

  (q/stroke 0 0 0)
  (q/fill 255 255 255)
  (q/ellipse 300 300 20 20)

  (dorun (for [pt (q/state :points)]
           (let [{[px py] :pos} @pt]
             (q/ellipse (+ 300 px) (+ 300 py) 10 10)
             (reset! pt (next-point @pt)))))

  (q/no-fill)
  (dorun (for [ring-atom (deref (q/state :rings))]
           (if ring-atom
             (let [[x y r] (deref ring-atom)
                   col (- 255 (* 2 r))]
               (if (< 20 col)
                 (q/stroke col 0 0)
                 (q/no-stroke))
               (q/ellipse (+ 300 x) (+ 300 y) r r)
               (reset! ring-atom [x y (+ 2 r)]))))))





(q/defsketch sketch-name
  :title "physdrone"
  :setup setup
  :draw draw
  :size [600 600]
  :renderer :opengl
  :target :perm-frame)
