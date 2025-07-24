(staple:load-system-quietly :clostrum-basic)
(staple:load-system-quietly :clostrum-trucler)

(defmethod staple:subsystems ((system (eql (asdf:find-system :clostrum))))
  (list (asdf:find-system :clostrum-basic)
        (asdf:find-system :clostrum-trucler)))

(defmethod staple:packages ((system (eql (asdf:find-system :clostrum))))
  ;; the implementation package is excluded,
  ;; as it is an implementation detail.
  (list (find-package "CLOSTRUM") (find-package "CLOSTRUM-SYS")))

(defmethod staple:packages ((system (eql (asdf:find-system :clostrum-basic))))
  (list (find-package "CLOSTRUM-BASIC")))
