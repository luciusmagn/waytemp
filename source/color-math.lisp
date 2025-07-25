(in-package #:waytemp)

(defconstant +pi+ (coerce pi 'double-float))

(defstruct xyz x y z)
(defstruct rgb r g b)

(defun illuminant-d (temp)
  "Daylight locus calculation for color temperature."
  (let ((x 0d0) (y 0d0))
    (cond
      ((and (>= temp 2500) (<= temp 7000))
       (setf x (+ 0.244063d0
                  (/ 0.09911d3 temp)
                  (/ 2.9678d6 (expt temp 2))
                  (- (/ 4.6070d9 (expt temp 3))))))
      ((and (> temp 7000) (<= temp 25000))
       (setf x (+ 0.237040d0
                  (/ 0.24748d3 temp)
                  (/ 1.9018d6 (expt temp 2))
                  (- (/ 2.0064d9 (expt temp 3))))))
      (t (error "Temperature ~D out of range [2500, 25000]" temp)))
    (setf y (+ (* -3 (expt x 2))
               (* 2.870 x)
               -0.275))
    (make-xyz :x x :y y :z (- 1.0 x y))))

(defun planckian-locus (temp)
  "Black body locus calculation for color temperature."
  (let ((x 0d0) (y 0d0))
    (cond
      ((and (>= temp 1667) (<= temp 4000))
       (setf x (+ (- (/ 0.2661239d9 (expt temp 3)))
                  (- (/ 0.2343589d6 (expt temp 2)))
                  (/ 0.8776956d3 temp)
                  0.179910d0))
       (if (<= temp 2222)
           (setf y (+ (* -1.1064814 (expt x 3))
                      (* -1.34811020 (expt x 2))
                      (* 2.18555832 x)
                      -0.20219683))
           (setf y (+ (* -0.9549476 (expt x 3))
                      (* -1.37418593 (expt x 2))
                      (* 2.09137015 x)
                      -0.16748867))))
      ((and (> temp 4000) (< temp 25000))
       (setf x (+ (- (/ 3.0258469d9 (expt temp 3)))
                  (/ 2.1070379d6 (expt temp 2))
                  (/ 0.2226347d3 temp)
                  0.240390d0))
       (setf y (+ (* 3.0817580 (expt x 3))
                  (* -5.87338670 (expt x 2))
                  (* 3.75112997 x)
                  -0.37001483)))
      (t (error "Temperature ~D out of range [1667, 25000]" temp)))
    (make-xyz :x x :y y :z (- 1.0 x y))))

(defun srgb-gamma (value gamma)
  "Apply sRGB gamma correction."
  (if (<= value 0.0031308d0)
      (* 12.92d0 value)
      (- (expt (* 1.055d0 value) (/ 1.0d0 gamma)) 0.055d0)))

(defun clamp (value)
  "Clamp value to [0,1] range."
  (max 0.0d0 (min 1.0d0 value)))

(defun xyz-to-srgb (xyz)
  "Convert XYZ color space to sRGB."
  (let ((x (xyz-x xyz))
        (y (xyz-y xyz))
        (z (xyz-z xyz)))
    (make-rgb
     :r (srgb-gamma (clamp (+ (* 3.2404542d0 x)
                              (* -1.5371385d0 y)
                              (* -0.4985314d0 z)))
                    2.2d0)
     :g (srgb-gamma (clamp (+ (* -0.9692660d0 x)
                              (* 1.8760108d0 y)
                              (* 0.0415560d0 z)))
                    2.2d0)
     :b (srgb-gamma (clamp (+ (* 0.0556434d0 x)
                              (* -0.2040259d0 y)
                              (* 1.0572252d0 z)))
                    2.2d0))))

(defun srgb-normalize (rgb)
  "Normalize RGB values so max component = 1.0"
  (let ((maxw (max (rgb-r rgb) (rgb-g rgb) (rgb-b rgb))))
    (make-rgb :r (/ (rgb-r rgb) maxw)
              :g (/ (rgb-g rgb) maxw)
              :b (/ (rgb-b rgb) maxw))))

(defun calc-whitepoint (temp)
  "Calculate RGB whitepoint for given color temperature."
  (if (= temp 6500)
      (make-rgb :r 1.0d0 :g 1.0d0 :b 1.0d0)
      (let ((xyz (cond
                   ((>= temp 25000) (illuminant-d 25000))
                   ((>= temp 4000) (illuminant-d temp))
                   ((>= temp 2500)
                    ;; Smooth transition between illuminant-d and planckian
                    (let* ((factor (/ (- 4000.0d0 temp) 1500.0d0))
                           (sinefactor (/ (+ (cos (* +pi+ factor)) 1.0d0) 2.0d0))
                           (xyz1 (illuminant-d temp))
                           (xyz2 (planckian-locus temp)))
                      (make-xyz :x (+ (* (xyz-x xyz1) sinefactor)
                                      (* (xyz-x xyz2) (- 1.0d0 sinefactor)))
                                :y (+ (* (xyz-y xyz1) sinefactor)
                                      (* (xyz-y xyz2) (- 1.0d0 sinefactor)))
                                :z (+ (* (xyz-z xyz1) sinefactor)
                                      (* (xyz-z xyz2) (- 1.0d0 sinefactor))))))
                   (t (planckian-locus (max temp 1667))))))
        (srgb-normalize (xyz-to-srgb xyz)))))

(defun fill-gamma-table (ramp-size r g b gamma)
  "Generate gamma correction table for given RGB values."
  (let ((table (make-array (* 3 ramp-size) :element-type '(unsigned-byte 16))))
    (dotimes (i ramp-size)
      (let ((val (/ (coerce i 'double-float) (1- ramp-size))))
        (setf (aref table i)
              (round (* 65535 (expt (* val r) (/ 1.0d0 gamma))))
              (aref table (+ i ramp-size))
              (round (* 65535 (expt (* val g) (/ 1.0d0 gamma))))
              (aref table (+ i (* 2 ramp-size)))
              (round (* 65535 (expt (* val b) (/ 1.0d0 gamma)))))))
    table))
