(staple:load-system-quietly :clostrum-basic)

(defmethod staple:subsystems ((system (eql (asdf:find-system :clostrum))))
  (list (asdf:find-system :clostrum-basic)))

(defmethod staple:packages ((system (eql (asdf:find-system :clostrum))))
  ;; the implementation package is excluded,
  ;; as it is an implementation detail.
  (list (find-package "CLOSTRUM") (find-package "CLOSTRUM-SYS")))

(defmethod staple:packages ((system (eql (asdf:find-system :clostrum-basic))))
  (list (find-package "CLOSTRUM-BASIC")))

(defmethod staple:documents ((system (eql (asdf:find-system :clostrum))))
  (list (asdf:system-relative-pathname system "../README.md")))
