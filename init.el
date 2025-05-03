;;; init.el --- The init loads confit.org

;;; Commentary:

;; Loads the org file of the literate config.

;;; Code:

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
;;; init.el ends here
