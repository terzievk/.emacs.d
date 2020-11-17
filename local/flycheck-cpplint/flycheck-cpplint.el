;;; flycheck-cpplint --- adds cpplint to flycheck checkers
;;; Commentary:
;;; minimal
;;; Code:

(require 'flycheck)
(require 'google-c-style)

(flycheck-define-checker cpplint
  "A C/C++ syntax checker using cpplint.

See URL `https://github.com/cpplint/cpplint'."
  ;; source-inplace instead of source because cpplint gets confused
  ;; by the flycheck temp files and prints wrong header guard messages
  :command ("cpplint" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes (c-mode c++-mode)
  :next-checkers ((warning . google-c-style)))

(add-to-list 'flycheck-checkers 'cpplint)

(provide 'flycheck-cpplint)
;;; flycheck-cpplint ends here
