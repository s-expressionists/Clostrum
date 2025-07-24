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
  ;; Shouldn't be necessary, but I'm trying to keep Staple from
  ;; generating a bunch of extraneous "English" links.
  (list (asdf:system-relative-pathname :clostrum "README.md")))

(defmethod staple:images ((system (eql (asdf:find-system :clostrum))))
  ;; None (try to avoid Staple putting in its own logo)
  ())
