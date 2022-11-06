(asdf:defsystem :my-algorithms
           :description "Algorithms written by my"
           :version "1.0"
           :author "Sergey Rukavishnikov"
           :components ((:file "package")
                        (:file "malg" :depends-on ("package"))))
                        
