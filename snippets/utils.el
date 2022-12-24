(defun split-by-snake (s)
  (split-string s "_"))

(defun snake-to-pascal-case (s)
  (mapconcat 'identity
    (mapcar #'capitalize (split-by-snake s))
    ""))

(defun capitalize-props (s)
  (concat
   (snake-to-pascal-case s)
   "Props"))
