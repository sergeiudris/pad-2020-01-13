(ns pad.math.core
  (:require [clojure.repl :refer :all]))


(def cprn prn)

(defn make-mx
  "Returns a mx given width, height and element"
  [wid hei el]
  (vec (repeat (* wid hei) el)))

(defn make-mx-with
  "Returns a matrix given width, height and make-el"
  [wid hei make-el]
  (->>
   (range (* wid hei))
   (map-indexed #(make-el %1  %2))
   vec))

(defn rows->mx
  "Returns a vector. flattens the A"
  [A]
  (->
   (flatten A)
   vec))

(defn mk-one-hot-vec
  [size idx]
  (->
   (repeat size 0.0)
   (vec)
   (assoc idx 1.0)))

#_(mk-one-hot-vec  5 2)

(defn mx->rows
  "Returns two dim vector of rows of A"
  [wid A]
  (->>
   (partition wid A)
   (mapv vec)))

(defn index->col
  "returns the col index given el index, row length"
  [i wid]
  (mod i wid))

(defn mx->cols
  "Returns two dim vector of cols of A"
  [wid A]
  (->>
   (persistent!
    (reduce-kv
     (fn [acc i x]
       (let [i-col (index->col i wid)]
         (assoc! acc i-col (conj (get acc i-col []) x)))
      ;
       )
     (transient {}) A))
   (into (sorted-map))
   vals
   vec
   ;
   ))

(comment

  (def A (rows->mx [[1 2 3]
                    [4 5 6]
                    [7 8 9]]))


  (->>
   (mx->cols 3 A)
   cprn
   ;
   )

  ;;;
  )

(defn prn-rows
  "Prtints rows, used by prn-mx"
  [wid A & {:keys [print-fn] :or {print-fn prn}}]
  (as-> nil R
    (mx->rows  wid A)
    (doseq [row R]
      ; (print)
      (print-fn row))))

(defn prn-mx
  "Prtints A"
  ([wid A]
   (prn-rows wid A)
   (println))
  ([wid A return?]
   (prn-rows wid A)
   (println)
   A))

(defn cprn-mx
  "Color-prtints A"
  ([wid A]
   (prn-rows wid A :print-fn cprn)
   (println))
  ([wid A return?]
   (prn-rows wid A :print-fn cprn)
   (println)
   A))

(defn cprn-rows
  "Color prints vec els per line"
  [xs]
  (doseq [x xs]
    (cprn x)))

(comment

  (rows->mx [[1 2 3]
            [4 5 6]])

  (cprn-mx 3 (rows->mx [[1 2 3]
                      [4 5 6]]))

  ;;;
  )

(defn index->row
  "Returns the row index given el index, row length"
  [i wid]
  (int (/ i wid)))




(defn index->pos
  "Returns the position [i,j] el in the mx of given width"
  [i wid]
  (vector (index->row i wid) (index->col i wid)))

(defn pos->index
  "Returns el index given row, col and row length"
  [i j wid]
  (->
   (* (inc i) wid)
   (- (- wid j))))


(defn mx->el
  "Returns the i,j el of A"
  [i j wid A]
  (A (pos->index i j wid)))

(defn index->el
  "Returns the el given index, wid "
  [wid i A]
  (mx->el (index->row i wid) (index->col i wid) wid A))

(defn mx->row
  "Returns i-th row of the mx"
  [i wid A]
  (->
   (keep-indexed  #(if (= (index->row %1 wid) i) %2 nil) A)
   vec))

(defn mx->col
  "Returns i-th row of the mx"
  [i wid A]
  (->
   (keep-indexed  #(if (= (index->col %1 wid) i) %2 nil) A)
   vec))

(comment
  
  (mx->row 1 3 [1 2 3 4 5 6] )
  (mx->col 1 3 [1 2 3 4 5 6])
  
  ;;;
  )

(defn make-vec
  "Returns a vector of specified size and el"
  [size el]
  (vec (repeat size el)))



(defn iden-mx
  "Returns an identity mx of order"
  [order]
  (let [A  (make-mx order order nil)]
    (->>
     A
     (map-indexed (fn mpr [i x]
                    (let [row-i (index->row i order)
                          col-i (index->col i order)]
                      (cond
                        (= row-i col-i) 1
                        :else 0))))
     vec)))

(defn iden-mx-3
  "Returns 3x3 identity mx"
  []
  (iden-mx 3))


(defn iden-mx-4
  "Returns 4x4 identity mx"
  []
  (iden-mx 4))


(defn diag
  "Returns a square mx with the vector as the diagonal "
  ([v]
   (->
    (let [size (count v)
          A    (make-mx size size 0)]
      (map-indexed (fn [i x]
                     (let [row-i (index->row i size)
                           col-i (index->col i size)]
                       (cond
                         (= row-i col-i) (nth v row-i)
                         :else x))) A))
    vec))
  ([wid hei v]
   (->
    (let [size (count v)
          A    (make-mx wid hei 0)]
      (map-indexed (fn [i x]
                     (let [row-i (index->row i wid)
                           col-i (index->col i wid)]
                       (cond
                         (and (<= row-i size) (= row-i col-i) (and (< row-i size))) (nth v row-i)
                         :else x))) A))
    vec)))

(comment

  (diag [1 2 3])
  (diag  4 4 [1 2 3])
  (get [1 2 3]  4)

  
  (iden-mx-4 )
  ;;;
  )


(defn elwise-prod
  "Returns a vector (mx), multiplies a,b element-wise "
  [a b]
  (mapv * a b))

(defn elwise-sum
  "Returns a vector (mx), adds a,b element-wise "
  [a b]
  (mapv + a b))

(defn elwise-subtract
  "Returns a vector (mx), subtracts a,b element-wise "
  [a b]
  (mapv - a b))

(defn elwise-divide
  "Returns a vector (mx), divides a,b element-wise "
  [a b]
  (mapv / a b))

(defn dot-prod
  "Retruns the sum of products of corresponding els"
  [a b]
  (reduce + 0 (map * a b)))

(defn scalar-prod
  "Returns vector of scalar el products"
  [x v]
  (mapv #(* x %)  v))

(defn scalar-divide
  "Returns vector, divides v by scalar element-wise"
  [x v]
   (mapv #(/ % x)  v)
  ; (prn x v)
  ; (mapv #(with-precision 10 (/ (bigdec %) (bigdec x)))  v)
  )

(comment
  (with-precision 10 (/ 0.81818181794497862030M 0.9045340337332909M))
  
  
  ;;;
  )


(defn scalar-add
  "Returns vector, adds a scalar element-wise"
  [x v]
  (mapv #(+ % x)  v))

(defn  scalar-subtract
  "Returns vector, subtracts a scalar element-wise"
  [x v]
  (mapv #(- % x)  v))

(defn mx-prod
  "Returns the product of multiplying AB.
   Left mx must have cols as right mx rows.
   Result mx has A-rows, B-cols
  "
  [wid-A wid-B A B]
  (let [hei-A (/ (count A) wid-A)
        M     (make-mx hei-A wid-B nil)]
    (->
     (map-indexed (fn [i x]
                    (let [row-i (index->row i wid-B)
                          col-i (index->col i wid-B)
                          row   (mx->row row-i wid-A A)
                          col   (mx->col col-i wid-B B)]
                      (dot-prod row col)
                      ;
                      ))
                  M)
     vec)))

(defn mx->order
  "Returns the order of a square mx"
  [A]
  (int (Math/sqrt (count A))))

(defn mx-prod+
  "Returns the product of multiplying n mxs"
  ([mxs]
   (mx-prod+ (mapv #(mx->order %) mxs) mxs))
  ([wids mxs]
   (reduce-kv (fn [R i A]
                (mx-prod (wids i) (wids (inc i)) R A))
              (first mxs) (vec (rest mxs)))))

(defn mx-pow
  "Returns A powers n"
  [n A]
  (mx-prod+ (vec (repeat n A))))

(comment

  (def A (rows->mx [[1 2 3]
                    [2 5 6]]))

  (def B (rows->mx [[0 1]
                    [2 3]
                    [4 5]]))

  (prn-mx 2 (mx-prod 3 2 A B))

  (def v [1 2 3 4])
  (reduce-kv (fn [acc i x]
               (prn i x)
               (+ acc x)) (first v) (vec (rest v)))

  (mapv (fn [_] 3) (range 2))

  (def C (rows->mx [[0 1 2]
                    [1 2 0]
                    [3 4 0]]))

  (def D (rows->mx [[0 1 3 4 5]
                    [2 3 7 1 2]
                    [4 5 0 1 2]]))

  (cprn-mx 3 (mx-pow 2 C))
  (cprn-mx 3 (mx-prod 3 3 C C))
  (cprn-mx 3 (mx-pow 3 C))
  (cprn-mx 3 (mx-prod+ [3 3 3] [C C C]))

  (cprn-mx 2 (mx-prod+ [3 3 2] [C A B]))
  (cprn-mx 5 (mx-prod+ [3 3 5] [C A D]))

  

  ;;;
  )

(defn vec-broadcast
  "Returns mx with vector added to each row element-wise"
  [wid A a]
  (as-> nil R
    (mx->rows wid A)
    (mapv #(elwise-sum a %)  R)))


(comment

  (def A [1 2 3 4 5 6])

  (def a [1 2 3])

  (vec-broadcast  3 A a)

  ;;;
  )




(defn vec-norm
  "Returns the  Euclidean norm (length or magnitude) of a vector.
   L2 norm"
  [a]
  ; (bigdec (Math/sqrt (dot-prod a a)))
   (Math/sqrt (dot-prod a a))
  )

(comment

  (def a  [7 2.01 3 5 3.149123123 0.189256])
  (bigdec 7)

  (vec-normalize a)
  (vec-norm a)
  (float  (dot-prod a a))
  (Math/sqrt (float  (dot-prod a a)))
  (bigdec (Math/sqrt (dot-prod a a)))
  (with-precision 5 (Math/sqrt (dot-prod a a)))
  (scalar-prod (vec-norm a) (vec-normalize a))

  (/ (bigdec 1) (bigdec 3))
  
  (.divide (bigdec 1) (bigdec 3) 10 RoundingMode.HALF_UP)
  
  (with-precision 50 :rounding HALF_DOWN (/ 1M 3M))
  
  (with-precision 50 :rounding HALF_DOWN (/ 1M 3))
  
  (with-precision 1 :rounding HALF_DOWN 3 (* 1 -1.4654943925052066E-14))
  
  (with-precision 13 (* (bigdec (Math/sqrt 2000000)) (bigdec (Math/sqrt 2000000))))
  
  (with-precision 18 (* (bigdec (Math/sqrt 2)) (bigdec (Math/sqrt 2))))
  
  ;;;
  )

(defn vec-L1-norm
  [a]
  "Returns the L1 norm, that grows at the same rate in all locations.
   Every time an element of x moves
   away from 0 by e , the L 1 norm increases by e"
  (->>
   a
   (mapv #(Math/abs %))
   (reduce +)))

(defn vec-max-norm
  "Returns the absolue value of the element with the largest magnitutde"
  [v]
  (->>
   v
   (mapv #(Math/abs %))
   sort
   last))

(defn mx-norm
  "Returns the norm (size) of a matrix.
   Frobenius size of A = sqrt of the sum of a[ij]^2  
  "
  [A]
  (vec-norm A))

(defn vec-normalize
  "Returns the unit vector (normalizes) of a"
  [a]
  (scalar-divide (vec-norm a) a))


(defn count-non-zero
  "Returns the count of non-zero elemts of a vector.
   Mistakenly called L0 norm.
   It's not a norm, scaling vector does not change the count of non-zero elems
  "
  [a]
  (->>
   a
   (filterv (fn [x] (not (zero? x))))
   count))

(comment
  (vec-norm [0 0 2])
  (vec-normalize [0 0 2])
  (count-non-zero [0 0 2])
  ;;;
  )

(defn mx-transpose
  "Returns transposed A - all indices are mirrored, e.g. 1,2 -> 2,1"
  [wid A]
  (let [len (count A)
        hei (/ len wid)]
    (reduce-kv (fn [acc i x]
                 (let [row-i (index->row i wid)
                       col-i (index->col i wid)
                       new-i (pos->index col-i row-i  hei)]
                   (assoc acc new-i x))) (make-mx hei wid nil) A)))


(defn cols->mx
  "Returns a vector. flattens the A"
  [A]
  (->>
   (flatten A)
   vec
   (mx-transpose (count (first A)))
   ))

(comment

  (cprn-mx 4 (cols->mx [[1 2 3]
                          [4 5 6]
                          [7 8 9]
                          [10 11 12]]))


  ;;;
  )

(defn mx-symmetric?
  "Returns true if matrix is equal to its own trasnpose"
  [A]
  (->
   (mx-transpose  (mx->order A) A)
   (= A)))

(defn vec-orthogonal?
  "returns true if both vectors are at 90 degree agle"
  [a b]
  (= (dot-prod a b) 0))

(comment
  (as-> nil R
    (make-mx-with 3 3  (fn [i x] i))
    (prn-mx 3 R true)
    (mx-transpose 3 R)
    (prn-mx 3 R )
    )
  
  
  (vec-orthogonal? [0 0 2] [1 0 0])
  ;;;
  )

(defn mx-reciprocal
  "returns the diag matrix with non-zero elems becoming 1/elem"
  [A]
  (mapv #(if (zero? %) %
            (/ 1 %)) A))

(defn mx-trace
  "Returns the sum of all diagonal entries of a matrix"
  [A]
  (let [order (mx->order A)]
    (->>
     A
     (keep-indexed #(if
                     (= (index->row %1 order) (index->col %1 order)) %2
                     nil))
     (reduce +))))

(defn vec-linear-comb
  "Returns a linear combination (sum of prodcuts)"
  [vs xs]
  (->>
   (mapv (fn [x v]
           (scalar-prod x v)) xs vs)
   (reduce elwise-sum)
   ;
   ))

(defn elwise-sum+
  "Returns the sum (linear combination) of vectors"
  [& vs]
  (reduce elwise-sum vs))

(defn elwise-subtract+
  "Returns the subtraction of vecs "
  [& vs]
  (reduce elwise-subtract vs))

(defn vecs-mean
  "Returns an average vec, is a linear comb"
  [& vs]
  (scalar-prod (/ 1 (count vs)) (apply elwise-sum+ vs)))

(defn inner-prod
  "Reutrns the sum of products of correspondin els"
  [a b]
  (dot-prod a b))

; affine  - sum of coefficeints in lin com is 1
; convex - affine and all coefs are positive

(defn inner-prod-stacked
  "Returns the inner prod of block vectors. Must have the same size (conform)"
  [a b]
  (reduce + (mapv inner-prod  a b)))

(comment

  (elwise-sum+ [1 2] [2 3] [2 2])

  (scalar-prod 1/3 [5 7])

  (vecs-mean [1 2] [2 3] [2 2])

  (inner-prod [-1 2 2] [1 0 -3])

  (inner-prod [2] [2])

  (inner-prod-stacked [[1 2] [3 4]] [[2 3] [1 1]])
  
  (float (inner-prod [3/4 1/8 1/8] [1 2 3]))

  (* 74 8) ; 74mb vec is 592 in mem
  

  ;;;
  )


(def e1 [1 0 0])

(def e2 [0 1 0])

(def e3 [0 0 1])


(defn nnz
  "Returns the count of non-zero els of a"
  [a]
  (count (filterv #(not (= 0 %)) a)))

(defn make-vec
  "Returns a vec given size, el"
  [size el]
  (vec (repeat size el)))

(defn vec-sum
  "Returns the sum of numbers.
   Sum is a linear combintaion of scalars"
  [a]
  (reduce + 0 a))

(defn fact
  "Returns factorial of x"
  [x]
  (reduce * 1 (range 1 (inc x))))

(defn vec-mean
  "Returns avg of a vector"
  [v]
  (/ (vec-sum v) (count v)))

(defn vec-max
  "Returns the max el of vec"
  [v]
  (apply max v))

(comment

  (nnz [1 2 0 0 5 6])
  (make-vec 3 0)
  (mx/iden-mx 3)
  
  (vec-mean [1 2 3])
  

  ;;;
  )

(defn vec-median
  "Returns the median of the vector"
  [a]
  (let [size (count a)
        v    (vec (sort a))]
    (cond
      (odd? size) (v (-> size dec (/ 2) ))
      :else (vec-mean (vector (v (-> size (/ 2) dec)) (v (/ size 2)))))))

(defn mean-square
  "Returns the sum of squared vec els, divided by el count"
  [a]
  (/ (dot-prod a a) (count a)))

(defn root-mean-square
  "Returns suqare root of mean-square.
   Tells what a 'typical'  |el| looks like"
  [a]
  (Math/sqrt (mean-square a))
  ; (/ (vec-norm a) (Math/sqrt (count) ) )
  )


(defn vec-norm-stacked
  "Returns the norm of a vector, formed from norms of subvecs"
  [aa]
  (vec-norm (mapv #(vec-norm %)  aa)))

(defn vec-dist
  "Returns the distance between two vecs (points in n dimension)"
  [a b]
  (vec-norm (elwise-subtract a b)))

(defn rms-deviation
  "Returns the root-mean-square of the difference a - b"
  [a b]
  (root-mean-square (elwise-subtract  a b)))

(defn rms-prediction-error
  "Returns the root-mean-square of y-ye"
  [y ye]
  (root-mean-square (elwise-subtract y ye)))

(defn nearest-neighbor
  "Returns the closest vec z (of zs) to vec x"
  [x zs]
  (as-> nil R
    (map-indexed #(vector %1 (vec-dist x %2))  zs)
    (sort-by second R)
    (ffirst R)
    (vector R (zs R))))


(comment

  (mx/elwise-sum [0 7 3] [1 2 0])

  (mx/scalar-add 2 [1 2 3])

  (mx/scalar-prod -2 [1 9 6])

  (mx/vec-linear-comb [[1 2 3] [0 0 1]] [1 2])

  (inner-prod [0.5 1.5] [0.481 0.736])

  (inner-prod [0.12 0.31 0.26] [0.5 1.1 0.3])
  (inner-prod  [0.12 0.31 0.26] [1.5 0.8 1.2])


  ; house prices regression model

  (+ (inner-prod [148.73 -18.85] [0.846 1]) 54.40)
  (inner-prod [1 148.73 -18.85] [54.40 0.846 1])

  (vec-mean [1 2])

  (vec-median [-7 -6 -5 -4 -3 1  2 3 4 7])

  (root-mean-square [1 2 3 4])

  (dot-prod [1 1 1 2 2 2]  [1 1 1 2 2 2])

  (/ (vec-norm [1 1 1 2 2 2]) (Math/sqrt 6))

  (root-mean-square [1 1 1 2 2 2])

  (vec-norm [1 1 1 1 1 1 1])

  (root-mean-square [1 1 1 1 1 1 1])

  (vec-norm-stacked [[1 0] [0 1]])

  (vec-norm [1 1])
    (prn R)    (prn R)

  (vec-dist [1 0 0] [3 0 0])

  (vec-dist [1 1 0] [2 2 0])


  (nearest-neighbor [5 6] [[2 1] [7 2] [5.5 4] [4 8] [1 5] [9 6]])

  ;;;
  )
(defn sq
  "Returns x squared"
  [x]
  (Math/pow x 2))

(defn sqrt
  "Returns x squared"
  [x]
  (Math/sqrt x))

(defn vec-demeaned
  "Returns the de-meaned vec - subtract vec-mean from each entry"
  [a]
  (elwise-subtract a (scalar-prod (vec-mean a) (make-vec (count a) 1))))

; 4 flops ?
(defn vec-standard-deviation
  "Returns root-mean-square of the de-meaned vec"
  [a]
  (root-mean-square (vec-demeaned a)))

; 3 flops ?
(defn vec-standard-deviation-2
  "Returns root-mean-square of the de-meaned vec"
  [a]
  (Math/sqrt (- (sq (root-mean-square a)) (sq (vec-mean a)))))

; https://github.com/numpy/numpy/blob/v1.17.0/numpy/core/fromnumeric.py#L3322
; The standard deviation is the square root of the average of the squared
; deviations from the mean, i.e., ``std = sqrt (mean (abs (x - x.mean ()) **2)) ``.

(defn std [a]
  (->>
   (scalar-subtract (vec-mean a) a)
   (map sq)
   (vec-mean)
   (Math/sqrt)))

(defn diff-max-min
  "Returns the diff of max nad min els"
  [a]
  (as-> nil R
    (sort a)
    (- (last R) (first R))))

(defn ==** 
  "Returns true, if diff between args is <= 0.00001"
  [& args]
  (apply == args)
  (<= (Math/abs (diff-max-min args)) 0.00001))

(defn ==*
  "Returns true, if diff between x and y is <= 0.00001"
  [x y]
  (<= (Math/abs (- y x)) 0.00001))

(comment

  (vec-mean (vec-demeaned [1 2 3]))

  (vec-standard-deviation (vec-demeaned [1 2 3]))
  ; same eas
  (rms-deviation [1 2 3] (mapv (fn [x] (vec-mean [1 2 3])) [1 2 3]))

  (vec-standard-deviation [2 2 2 2 2])
  (def a [1 -2 3 2])
  (vec-demeaned a)
  (vec-mean a)
  (format  "%.3f"  (vec-standard-deviation a))
  (vec-standard-deviation a)
  (vec-standard-deviation-2 a)
  (root-mean-square (vec-demeaned a))

  (source ==)

  (==* 1 1.0 1)

  (vec-mean [5 4.999999])
  (vec-demeaned [5 4.999999])

  (==* 4.9999999M 4.9999999)

  (diff-max-min [4 3 2 -1 11])

  (==** 5 4.99999)

  (==* -4.999999 -5M)

  ;;;
  )


(defn vec-standardized
  "Returns de-meaned vector, divided by standard deviation"
  [a]
  (scalar-divide (vec-standard-deviation a) (vec-demeaned a)))

(defn rad->deg
  "Returns degrees (from value in radians)"
  [x]
  (* x (/ 180 Math/PI)))

(defn deg->rad
  "Returns radians (from value in degrees)"
  [x]
  (/ (* x Math/PI) 180))

(defn vec-angle
  "Returns the angle (radians) between two non-zero vecs"
  [a b]
  (Math/acos (/ (inner-prod a b) (* (vec-norm a) (vec-norm b)))))

(defn spherical-dist
  "Returns the dist between two 3-vecs on a spehre of radius r"
  [r a b]
  (* r (vec-angle a b)))

(defn latlon->3-D
  "Returns the 3-D coords for lat lon, given radius "
  [r lat lon]
  (let [lat-rad (deg->rad lat)
        lon-rad (deg->rad lon)]
    [(* r (Math/sin lon-rad) (Math/cos lat-rad))
     (* r (Math/cos lon-rad) (Math/cos lat-rad))
     (* r (Math/sin lat-rad))]))

(defn vec-correlation-coef
  "Returns the correlation coefficient (cos of angle) between de-meaned vecs "
  [a b]
  (let [[ad bd] [(vec-demeaned a) (vec-demeaned b)]]
    (/ (inner-prod ad bd) (* (vec-norm ad) (vec-norm bd)))
    ; (Math/cos (vec-angle ad bd))
    ))

(comment

  (def x-standardized (vec-standardized [1 2 3  5]))

  (vec-mean x-standardized)
  (vec-standard-deviation x-standardized)

  (vec-correlation-coef [1 0 0] [-1 0 0])

  (vec-correlation-coef [1 0 0] [2 0 0])

  (rad->deg 0.9661)
  (deg->rad 55.35)

  (def Earth-radius 6367.5)

  (def Beijing (latlon->3-D Earth-radius 39.914 116.392))
  (def Palo-Alto (latlon->3-D Earth-radius 37.429 -122.138))

  (spherical-dist Earth-radius Beijing  Palo-Alto) ; 9093 km


  ;;;
  )


(defn gram-schmidt-qi
  "Returns the vec q ith, checks for orthogonalization"
  [x qs]
  ; (prn x)
  ; (cprn qs)
  (->>
   (pmap (fn [q]
           (scalar-prod  (inner-prod q x) q))  qs)
   vec
   (reduce-kv (fn [acc i q*]
                (elwise-subtract acc q*)) x)
   vec
   ;
   ))

(defn gram-schmidt
  "Returns true if vecs are linearily independent"
  ([xs]
   (gram-schmidt xs []))
  ([xs qs]
   (let [xi (first xs)
         qi (gram-schmidt-qi xi qs)]
    ;  (cprn xi)
     (prn qi)
     (cprn (vec-normalize qi))
    ;  (prn)
     (cond
       (empty? xs) qs
       (every? zero? qi)  false
       :else (gram-schmidt (vec (rest xs)) (conj  qs (vec-normalize qi)))))
   ;
   ))

(comment
  (gram-schmidt [[1 0] [2 0]])
  (gram-schmidt [[2 1] [1  0] [0 1]])
  (gram-schmidt [[1 1] [-3 2] [2 4]]) ; should be dependent
  (gram-schmidt [[1 4 2 -3] [7 10 -4 -1] [-2 1 5 -4]])
  
  
  (def a1 [-1 1 -1 1])
  (def a2 [-1 3 -1 3])
  (def a3 [1 3 5 7])

  (def qs (gram-schmidt [a1 a2 a3]))
  (def Q (cols->mx  qs))
  (cprn-mx 3 Q)
  (cprn-mx 3 (mx-prod 4 3 (mx-transpose 3 Q) Q)) ; should be Identity and it is!

  (def a [1 1 1])
  (def b [1 2 0])
  (def c [0 -1 1])

  (gram-schmidt [a b c])

  (every? zero? [0.0 0.0 0.0 -0.0])


  ;;;
  )


(comment

  (gram-schmidt [[1 0 0] [0 1 0] [0 0 1]])

  (gram-schmidt [[2 1 0] [1 3 0] [0 0 1]])

  (gram-schmidt [[2 1] [1  0] [0 1]])

  (vec-normalize [2 1])

  (elwise-subtract [1  0] (vec-normalize [2 1]))

  (elwise-subtract   [0 1 0]  (scalar-prod (inner-prod [1 0 0] [0 1 0]) [1 0 0]))
  (vec-normalize [-1 1 0])

  (def a1 [-1 1 -1 1])
  (def a2 [-1 3 -1 3])
  (def a3 [1 3 5 7])



  (vec-norm a1)
  (def q1 (vec-normalize a1))
  (def q2- (elwise-subtract a2 (scalar-prod  (inner-prod   q1 a2) q1)))
  (def q2 (vec-normalize q2-))
  (def q3- (elwise-subtract+ a3 (scalar-prod  (inner-prod   q1 a3) q1)  (scalar-prod  (inner-prod   q2 a3) q2)))
  (def q3 (vec-normalize q3-))

  (gram-schmidt [a1 a2 a3])

  (gram-schmidt [[1 1 1]  [2 2 2] [3 3 3]])

  (gram-schmidt [[1 1] [2 2]])

  (gram-schmidt [[1 0] [2 0]])


  (every? zero? (elwise-sum+ (scalar-prod -1/2 [2 2 2])  (scalar-prod -2 [1 1 1]) [3 3 3]))

  (every? zero? (elwise-sum+ (scalar-prod -1 [2 0 0])  (scalar-prod -1 [1 0 0]) [3 3 0]))

  (gram-schmidt [[1.2 -2.6] [-0.3 -3.7]])


  (def a [1 1 1])
  (def b [1 2 0])
  (def c [0 -1 1])


  (gram-schmidt [a b c])

  (def q1 (vec-normalize a))
  (def q2 (as-> nil R
            (inner-prod q1 b)
            (scalar-prod R q1)
            (elwise-subtract b R)
            (vec-normalize R)))
  
  (def q3 (as-> nil R
            [ (scalar-prod (inner-prod q1 c) q1) (scalar-prod (inner-prod q2 c) q2) ]
            
            (elwise-subtract+ c (first R) (second R))
            (vec-normalize R)))

  
  (vec-normalize [0 0 0])
  (elwise-subtract+ [-1 3 -1 3] [-2 2 -2 2] )
  
  ;;;
  )



; An n-vector can be interpreted as an n × 1 matrix;

(defn mx->density
  "Returns densitry of the mx at (0,1)"
  [A]
  (/ (count-non-zero A) (count A)))

(comment

  (mx->density (iden-mx 4))

  (diag (make-vec 4 1))

  (def A (rows->mx [[1 2 3]
                    [2 1 4]
                    [3 4 1]]))
  (def B (rows->mx [[3 1 0]
                    [2 1 4]
                    [-1 9 3]]))

  (mx-symmetric? A)

  (cprn-mx 3 A)

  (cprn-mx 3 (mx-transpose 3 A))


  (=   (mx-transpose 3 (elwise-sum A B))
       (elwise-sum (mx-transpose 3 A) (mx-transpose 3 B)))

  (= (vec-norm A) (mx-norm A))

  

  ;;;
  )

(defn mx-rms
  "Returns the root-mean-sqaure of mx els"
  [A]
  (/ (mx-norm A) (sqrt (count A))))

(defn mx-dist
  "Returns the distance between two mx"
  [A B]
  (vec-dist A B))



(defn mx-vec-prod-cols
  "Returns the mx vec prod using cols interpretation"
  [A x]
  (let [wid  (count x)
        cols (mx->cols wid A)]
    (vec-linear-comb cols x)))

(defn mx-vec-prod-rows
  "Returns the mx vec prod using cols interpretation"
  [A x]
  (let [wid  (count x)
        rows (mx->rows wid A)]
    (mapv #(inner-prod % x) rows) ))

(comment

  (def A (rows->mx [[1 2 3]
                    [2 1 4]
                    [3 4 1]]))
  (def B (rows->mx [[3 1 0]
                    [2 1 4]
                    [-1 9 3]]))

  (mx-rms A)
  (mx-rms B)

  (def C [0 2 -1 -2 1 1])
  (def c [2 1 -1])
  
  
  (mx-prod 3 1 C c)

  (mx-vec-prod-cols C c  )
  
  (mx-vec-prod-rows C c)
  
  (mx-vec-prod-cols C [0 1 0])
  


  ;;;
  )

(defn make-diff-mx
  "Returns (n-1) x n  difference mx "
  [wid]
  (let [hei (dec wid)
        A   (make-mx wid hei nil)]
    (->>
     (map-indexed (fn [i x]
                    (let [i-row (index->row i wid)
                          i-col (index->col i wid)]
                      (cond
                        (= i-row i-col) -1
                        (= (inc i-row) i-col ) 1
                        :else 0))) A)
     vec)))

(defn make-running-sum-mx
  "Returns nxn running sum matrix"
  [order]
  (let [A   (make-mx order order nil)]
    (->>
     (map-indexed (fn [i x]
                    (let [i-row (index->row i order)
                          i-col (index->col i order)]
                      (cond
                        (<= i-col i-row) 1
                        :else 0))) A)
     vec)))

(defn mx-reverse-cols
  "Returns mx with cols reversed"
  [wid A]
  (->>
   (mx->cols wid  A)
   reverse
   cols->mx))

(defn make-reverser-mx
  "Returns nxn running sum matrix"
  [order]
  (->>
   (iden-mx order)
   (mx-reverse-cols order)))

(defn make-gram-mx
  "Returns the Gram matrix"
  [wid A]
  (mx-prod (/ (count A) wid) wid (mx-transpose wid A) A))

(comment

  (cprn-mx 6 (make-diff-mx 6))

  (cprn-mx 6 (make-running-sum-mx 6))


  (def a [1 1 1])
  (def b [1 2 0])
  (def c [0 -1 1])

  (def A (rows->mx [[1 1 0]
                    [1 2 -1]
                    [1 0 1]]))

  (def a1 [-1 1 -1 1])
  (def a2 [-1 3 -1 3])
  (def a3 [1 3 5 7])

  (def A (rows->mx [[-1 -1 1]
                    [1 3 3]
                    [-1 -1 5]
                    [1 3 7]]))


  (cprn-mx 4 (mx-transpose 4 (iden-mx 4)))
  
  (cprn-mx 3 (mx-reverse-cols 3 A))

  (cprn-mx 7 (make-reverser-mx 7))
  (cprn-mx 4 (mx-transpose 3 A))
  (cprn-mx 3 (make-gram-mx 3 A) )
  
  ;;;
  )

(defn xor
  "Returns true only if one form is true"
  [& fs]
  (cond
    (empty? fs) false
    (first fs) (not (apply xor (rest fs)))
    :else (apply xor (rest fs))))

(defn =*
  "Returns true if vecs are equal"
  [a b]
  (zero? (compare a b)))

(defn =**
  "Returns true if vecs are equal with comparer (==*) "
  ([a b]
   (=** a b ==*))
  ([a b comparer]
   (cond
     (and (number? a) (number? b)) (comparer a b)
     (and (empty? a) (empty? b)) true
     (empty? a) false
     (xor (coll? (first a)) (coll? (first b))) false
     :else (and (=** (first a) (first b) comparer) (=** (rest a) (rest b) comparer))
     ;
     )))

(comment

  (=* [1] [1])

  (=** [1 2 [3 4.00000001 5 [6]]] [1 2 [3 4 5 [5.9999]]])

  (xor false  false nil)
  
  (xor false  false nil false [] 'a)
  

  ;;;
  )

; UNFINISHED
(defn vec-conv
  "Returns convoluted vector"
  [a b]
  (let [size-a (count a)
        size-b (count b)
        size   (dec (+ size-a size-b))
        c      (make-vec size nil)]
    (mapv (fn [i]) c)))

(defn outer-prod
  "Returns a mx of outer prod of two vecs"
  [a b]
  (mx-prod 1 (count b) a b))

;FIX equality
(defn mx-orthogonal?
  "Returns true if A's cols are an orthonormal basis"
  [A]
  (let [order (mx->order A)]
    (=* (mx-prod order order (mx-transpose order A) A) (iden-mx order))))


(defn scalar-inverse
  "Returns (/ 1 x)"
  [x]
  (/ 1 x))

(defn mx-left-inverse?
  "Returns true if X is the left inverse of A"
  [widX widA X A]
  (=* (mx-prod widX widA X A) (iden-mx widA)))

(defn mx-right-inverse?
  "Returns true if X is the right inverse of A.
  Any X has the same dimensions as A^T"
  [widX widA X A]
  (=* (mx-prod widA widX A X) (iden-mx widX)))







(comment

  (mapv (fn [x y]
          (prn x y)) [1 2 3 4] [0 -1 -2])

  (cprn-mx 3 (outer-prod [1 2 3 4 5] [4 5 6]))

  (mx-orthogonal? (rows->mx [[0 0 1.0]
                             [1 0 0]
                             [0 1 0]]))

  (source =)

  (. clojure.lang.Numbers (equiv 1 1.0))
  (. clojure.lang.Numbers equiv 1 1.0)
  (== 1 1.0)

  (compare [1 1] [1 1])
  (compare [1 1 2] [1 1.0])

  (compare [1 1 [1 2]] [1 1 [1 2.0]] [1 1 [1.0 2.0M]])

  (=* [1 2 3] [1.0 2N 3.0M])

  (def A (rows->mx [[-3 -4]
                    [4 6]
                    [1 1]]))

  (def B (rows->mx [[0 -0.5 3]
                    [0 0.5 -2]]))
  
  (def C (scalar-prod 1/9 (rows->mx [[-11 -10 16]
                                     [7 8 -11]]) ))

  (mx-left-inverse? 3 2  B A )
  
  (mx-left-inverse? 3 2  C A)
  
  (mx-right-inverse? 2 3  (mx-transpose 3 C) (mx-transpose 2 A))
  
  

  (mx-prod 3 2 B A)
  ;;;
  )


(defn back-substitution-xi
  "Returns the value of x ith"
  [R bi xs]
  (let [order (mx->order R)
        i-b   (dec (- order (count xs)))]
    (as-> nil E
      (reduce-kv (fn [acc i x]
                   (->>
                    (mx->el  i-b (+ i-b i 1)  order R)
                    (* x)
                    (+ acc))
                       ;
                   )0 xs)
      (- bi E)
      (/ E (mx->el  i-b i-b order R)))))

(defn back-substitution
  "Return vec x.
   Rx = b
  "
  ([widR R bs]
   (back-substitution widR R bs []))
  ([widR R bs xs]
   (cond
     (empty? bs) xs
     :else (recur widR
                  R
                  (vec (drop-last bs))
                  (vec (cons  (back-substitution-xi R (last bs) xs) xs))))))


(comment

  (def A (rows->mx [[1 2 3]
                    [0 -1 2]
                    [0 0 4]]))
  (mx->el (dec 3) (dec 3) 3 A)

  (def bs [-1 3 2])

  (back-substitution 3 A bs [])
  (back-substitution 3 A bs)
  
  (mx-prod 3 1 A [3/2 -2 1/2] )
  
  (back-substitution 3 A [-1 3 2 4])
  

  (def B (rows->mx [[1 -1 2 4]
                    [0 2 3 5]
                    [0 0 -1 -3]
                    [0 0 0 1]]))
  
  (def bs2 [1 3 -1 2])
  
  (back-substitution 4 B bs2 [])
  
  ;;;
  )

(defn gram-schmidt-q
  "Returns qi as aprt of orthonogalization step"
  [x qs]
  (as-> nil E
    (reduce-kv (fn [acc i q]
                 (->>
                  (scalar-prod (inner-prod q x) q)
                  (elwise-subtract acc)
                  ;
                  )) x qs)))

(defn gram-schmidt-qs
  "Returns [qs- qs] an orthonormal collection of vectors qi..qk.
   Each xi is a linear comb of qs and each qi is a lincomb of xi..xk.
    "
  ([xs]
   (gram-schmidt-qs xs [] []))
  ([xs qs- qs]
   (if (empty? xs) [qs- qs]
       (let [xi (first xs)
             qi (gram-schmidt-q xi qs)]
        ;  (prn xi qs qi)
         (cond
          ;  (every? #(==* 0 %) qi) (do (cprn qi) false)
           (every? #(== 0 %) qi) (do (cprn qi) false)
           :else (recur (vec (rest xs))
                        (vec (conj qs- qi))
                        (vec (conj qs (vec-normalize qi))))
         ;
           )))))

(comment
  
  (def A (rows->mx [[1 2 3]
                    [4 5 6]
                    [7 8 9]]))
  
  (gram-schmidt-qs (mx->cols (mx->order A) A))
  
  ;;;
  )

(defn mx->qs
  "Returns [qs- qs] the Q mx with cols as orthonormal vecs prodcued by applying Gram-Schmidt
   to A's cols.
   "
  [wid A]
  (as-> nil E
    (mx->cols wid A)
    (gram-schmidt-qs E)))

(defn QR-factorization->Q
  "Returns Q mx formed by Gram-Schmidt alg"
  [wid A]
  (as-> nil E
    (mx->cols wid A)
    (second (gram-schmidt-qs E))
    (cols->mx  E)))

(defn QR-factorization->R
  "Returns the R triangular mx for A = QR"
  [widA A]
  (let [widR     widA
        R        (make-mx widR widR nil)
        xs       (mx->cols widA A)
        [qs- qs] (mx->qs widA A)]
    (->
     (map-indexed (fn [i _]
                    (let [i-row (index->row i widR)
                          i-col (index->col i widR)
                          qi    (qs i-row)
                          xj    (xs i-col)
                          qi-   (qs- i-row)]
                      (cond
                        (= i-row i-col) (vec-norm qi-)
                        (< i-row i-col)  (inner-prod qi xj)
                        (> i-row i-col) 0))) R)
     vec)))


(comment
  (gram-schmidt-qs [[1 0] [2 0]])
  (gram-schmidt-qs [[2 1] [1  0] [0 1]])
  (gram-schmidt-qs [[1 1] [-3 2] [2 4]]) ; should be dependent
  (gram-schmidt-qs [[1 4 2 -3] [7 10 -4 -1] [-2 1 5 -4]])



  (def a1 [-1 1 -1 1])
  (def a2 [-1 3 -1 3])
  (def a3 [1 3 5 7])

  (def A (cols->mx [a1 a2 a3]))
  (cprn-mx 3 A)
  (def qs (second (gram-schmidt-qs [a1 a2 a3])))
  (def Q (cols->mx  qs))
  (cprn-mx 3 Q)
  (cprn-mx 3 (mx-prod 4 3 (mx-transpose 3 Q) Q)) ; should be Identity and it is

  (mx->cols 3 A)
  (gram-schmidt-qs (mx->cols 3 A))
  (cprn-mx 3 (cols->mx (mx->qs 3 A)))


  (def R (QR-factorization->R 3 A))
  (cprn-mx 3 R)
  (cprn-mx 3 (mx-prod 3 3 Q R))
  (=* A (mx-prod 3 3 Q R))

  (=* A (mx-prod 3 3 Q R))

  (cprn-mx 3 (QR-factorization->Q 3 A))

  (=* A (mx-prod 3 3 (QR-factorization->Q 3 A) (QR-factorization->R 3 A)))


  (cprn-mx 3 (mx-prod 3 3 Q (rows->mx [[2 4 2]
                                       [0 2 8]
                                       [0 0 4]])))

  (def a [1 1 1])
  (def b [1 2 0])
  (def c [0 -1 1])

  (vec-norm [-0.5 0.5 -0.5 0.5])

  (gram-schmidt-qs [a b c])

  (every? zero? [0.0 0.0 0.0 -0.0])



  ;;;
  )

(defn mx-inverse
  "Returns the inverse of A"
  ([A]
   (mx-inverse A (mx->order A)))
  ([A widA]
   (let [     
        ; [qs- qs] (mx->qs widA A)
         heiA     (/ (count A) widA)
         Q        (QR-factorization->Q widA A)
         Q'T      (mx-transpose widA Q)
         Q'T-cols (mx->cols heiA Q'T)
         R        (QR-factorization->R widA A)]
    ; (prn widA)
    ; (cprn-mx widA R)
    ; (cprn-mx widA Q)
    ; (cprn-mx widA Q'T)
     (as-> nil E
       (map-indexed (fn [idx q]
                    ; (prn (back-substitution widA R q))
                      (->
                       (back-substitution widA R q)
                    ;  reverse
                       vec)) Q'T-cols)
    ;  (do (prn E) E)
       (cols->mx E))
    ;
     )))

(defn mx-pseudo-inverse
  "Returns the inverse of A"
  [widA A]
  (mx-inverse A widA))


(defn make-vandermode-mx
  "Returns Vandermonde mx
  1 t1 .. t1^(n-2) t1^(n-1)
  1 t2 .. t2^(n-2) t2^(n-1)
  ...
  "
  [xs]
  (let [hei (count xs)
        A   (make-mx hei hei nil)]
    (->
     (map-indexed  (fn [idx _]
                     (let [[i j] (index->pos idx hei)]
                       (prn i j)
                       (cond
                         (= j 0) 1
                         :else (Math/pow (xs i) j)))) A)
     vec)))

(defn linear-equation-inverse
  "Returns the solution to
  x = A^-1b 
  "
  [A b]
  (let [order (mx->order A)]
    (as-> nil E
      (mx-inverse A)
      (mx-prod order 1 E b))))


(comment

  (def V (make-vandermode-mx [-1.1 -0.4 0.2 0.8]))
  (prn-mx 4 V)

  (Math/pow 0.2 3)
  (Math/pow -0.4 3)
  (Math/pow 0.8 3)

  (prn-mx 4 (mx-inverse V))
  

  ; Lagrange polynomials, associated with 
  ; points −1.1, −0.4, 0.2, 0.8. Coeffs are cols of A^-1
  (linear-equation-inverse V [1 0 0 0])
  (linear-equation-inverse V [0 1 0 0])
  (linear-equation-inverse V [0 0 1 0])
  (linear-equation-inverse V [0 0 0 1])


  ;;;
  )


(comment


  (def a1 [-1 1 -1])
  (def a2 [-1 3 -1])
  (def a3 [1 3 5])

  (def A (cols->mx [a1 a2 a3]))

  (def Q (QR-factorization->Q 3 A))
  (cprn-mx 3 Q)
  (=* (cprn-mx 3 (mx-prod 3 3 (mx-transpose 3 Q) Q)) (iden-mx 3))

  (def R (QR-factorization->R 3 A))
  (cprn-mx 3 R)

  (=* A (mx-prod 3 3 Q R))
  (def A' (mx-prod 3 3 Q R))
  (cprn-mx 3 A')

  (cprn-mx 3 (mx-inverse A))

  (cprn-mx 3 (mx-prod 3 3  A (mx-inverse A)))

  (=** (mx-prod 3 3  A (mx-inverse A)) (iden-mx 3))
  
  (= (mx-prod 3 3  A (mx-inverse A)) (iden-mx 3))

  (=** [1] [1])
  
  -2.220446049250313E-16
  
  ;;;
  )

(defn QR-linear-equation
  "Returns the result of solving
  Ax = b
  using QR-factorization.
  "
  ([A b]
   (QR-linear-equation (mx->order A) A b))
  ([wid A b]
   (let [hei (/ (count A) wid)
         Q      (QR-factorization->Q wid A)
         Q'T    (mx-transpose wid Q)
         Q-cols (mx->cols hei Q'T)
         R      (QR-factorization->R wid A)
         Q'Tb   (mx-prod hei 1 Q'T b)]
     (back-substitution wid R Q'Tb))))


(defn factor-solve-multiple
  "Returns cols (X), solution to 
  AX = B.
  Uses factorization caching.
  "
  ([widB A B]
   (factor-solve-multiple (mx->order A) widB A B))
  ([widA widB A B]
   (let [hei    (/ (count A) widA)
         Q      (QR-factorization->Q widA A)
         Q'T    (mx-transpose widA Q)
         Q-cols (mx->cols hei Q'T)
         R      (QR-factorization->R widA A)
         bs     (mx->cols widB B)
         heiB   (/ (count B) widB)
         X      (make-mx widB heiB nil)]
    ; (prn bs)
     (->
      (map-indexed (fn [i b]
                     (back-substitution widA R (mx-prod hei 1 Q'T b))) bs)
      vec))))

(comment

  (def A (rows->mx [[0 2 -1]
                    [0 5 1]
                    [3 0 4]]))

  (def B (rows->mx [[1 0 2]
                    [3 4 -1]
                    [5 1 0]]))

  (QR-linear-equation B [3 6 -3])
 ; [-1.1818181818181819 2.9090909090909096 2.0909090909090913]  
 ; correct 
  (def C (rows->mx [[3 6 -3 -2]
                    [-1 2 4 1]
                    [2 0 2 5]]))

  (prn-mx 4 (cols->mx (factor-solve-multiple 4 B C)))
  (QR-linear-equation B [3 -1 2])
  (QR-linear-equation B [6 2 0])
  

  (def x puget.printer/*options*)

  (def ^:dynamic *opts* x)
  
  (binding [*opts* puget.printer/*options*]
    (set! *opts* (merge x {:width 100})))

  puget.printer/*options*
  
  (puget.printer/merge-options  puget.printer/*options* {:width 100} )


  ;;;
  )



(comment
  ; Balancing chemical reactions.

  (def R (rows->mx [[2 0 0]
                    [7 0 0]
                    [0 1 0]
                    [0 0 1]
                    [-2 2 1]]))
  
  (def P (rows->mx [[1 0 0]
                    [0 0 1]
                    [0 1 0]
                    [0 0 2]
                    [3 3 0]]))
  
  (def A (rows->mx [[2 0 0 -1 0 0]
                    [7 0 0 0 0 -1]
                    [0 1 0 0 -1 0]
                    [0 0 1 0 0 -2]
                    [-2 2 1 -3 -3 0]
                    [1 0 0 0 0 0]]))
  
  (def bs [0 0 0 0 0 1])
  
  (mx-prod 6 1 (mx-inverse A) bs )
  (mx->order A)
  
  (linear-equation-inverse A bs)
  (QR-linear-equation A bs) ; correct [1.0000000000000007
                           ;           5.999999999999924
                            ;          13.999999999999995
                             ;         2.000000000000012
                              ;        5.999999999999933
                               ;       7.000000000000001]

  ;;;
  )


(defn mx-invertible?
  "Returns true if A is invertible (nonsingular)"
  [A]
  (boolean (gram-schmidt-qs (mx->cols (mx->order A) A))))

(defn mx-linearly-indep-cols?
  "Returns true of mx has linearly independent columns"
  [wid A]
  (let [hei (/ (count A) wid)]
    (mx-invertible? (make-gram-mx wid A ))))

(defn mx-singular?
  "Returns true if mx has linearly dependent cols"
  [wid A]
  (not (mx-linearly-indep-cols? wid A)))

(comment

  (def A (rows->mx [[1 2 3]
                    [4 5 6]
                    [7 8 9]]))

  (gram-schmidt-qs (mx->cols  3  A))

  (mx-inverse A)

  (mx-invertible? A)

  (def a1 [-1 1 -1 1])
  (def a2 [-1 3 -1 3])
  (def a3 [1 3 5 7])

  (def A (cols->mx [a1 a2 a3]))
  (cprn-mx 3 A)
  (def qs (second (gram-schmidt-qs [a1 a2 a3])))
  (def Q (cols->mx  qs))
  (cprn-mx 3 Q)
  (cprn-mx 3 (mx-prod 4 3 (mx-transpose 3 Q) Q))

  (def ATA (mx-prod  4  3 (mx-transpose 3 A) A))

  (prn-mx 3 ATA)

  (mx-invertible? ATA)

  (boolean [])

  (mx-linearly-indep-cols? 3 A)

  (mx-singular? 3 A)




  ;;;
  )


(defn linear-equation-pseudo-inverse
  "Returns the solution of
  Ax = b
  "
  [wid A b]
  (as-> nil E
    (mx-pseudo-inverse wid A)
    (mx-prod (/ (count A) wid) 1 E b)))


(defn sign
  "Returns +1 if x >= 0, -1 if x < 0"
  [x]
  (cond
    (>= x 0) +1
    :else -1))

(comment

  (def A (rows->mx [[-3 -4]
                    [4 6]
                    [1 1]]))

  (def Q (QR-factorization->Q 2 A))
  (prn-mx 2 Q)

  (def R (QR-factorization->R 2 A))
  (prn-mx 2 R)

  (prn-mx 3 (mx-pseudo-inverse 2 A))

  (linear-equation-pseudo-inverse 2 A [1 -2 0])

  (=** (linear-equation-pseudo-inverse 2 A [1 -2 0]) [1 -1]) ; true



  (sign -7)
  (sign 0)
  (sign 1)


  ;;;
  )

(defn make-regression-model-feature-mx
  [cols]
  (as-> nil E
    (mapv #(vec (cons 1 %)) cols)
    (cols->mx E)))

(defn regression-model-parameters
  "Returns the coeffs and v values"
  [widX X ys]
  (as-> nil E
    (QR-linear-equation (/ (count X) widX) (mx-transpose widX X)  ys)))

(defn regression-model-predictions
  "Returns the y vec"
  [widX X- bs-]
  (let [heiX (/ (count X-) widX)]
    (mx-prod heiX 1 (mx-transpose widX X-) bs-)))

(defn error-rate
  "Returns the total num of errors divided by total num of examples"
  [y y-]
  (->
   (count (->> (mapv #(if (not= %1 %2) 1 nil)  y y-) (filterv identity ) ))
   (/ (count y))))

(comment
  
  (error-rate [1 1 1] [1 0 1])
  (filterv identity [nil 1 nil])
  (count (->> (mapv #(if (not= %1 %2) 1 nil) [1 1 1] [1 0 1]) (filterv identity)))
  (filterv identity [nil 1 nil])
  ;;;
  )

(defn vec-nnz-indices
  "Returns indices of non-zero els
   "
  [xs]
  (vec (keep-indexed #(if (not (== 0 %2)) %1 nil) xs)))

(defn vecs-nnz-indices
  "Returns indices vec of  els
   that are non-zero in at least one of vecs
   "
  [vecs]
  (->
   (reduce-kv (fn [acc i x]
                (apply conj acc (vec-nnz-indices x)))  (sorted-set) vecs)
   ))

(comment
  
  ((sorted-set 3) 11)
  
  (vec-nnz-indices [1 2 0 0 3 5 7 8 0 0 0  0 0 0 0 0 0 0 0 0 ])
  
  (vecs-nnz-indices [[1 2 0 0 3] [1 2 0 0 5 ] [0 0 0 0 0]] )
  
  (apply conj #{ 1 2 3} [1 2 3 5])
  
  ;;;
  )

(defn agrmax
  "Returns the index of the largest value amonng the numbers"
  [xs]
  (->>
   (map-indexed #(vector %1 %2) xs)
   (sort-by second)
   last
   first))

(comment

  (agrmax [14 1 9 8 13 11 7 8 4 5 12])

  ;;;
  )

(defn cos-sim
  [a b]
  (/ (dot-prod a b) (* (vec-norm a) (vec-norm b))))

#_(cos-sim [1 2] [10 20])
#_(cos-sim [1 2] [-1 -2])


