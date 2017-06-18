;;; ginkgo-mode.el --- Helper functions for Ginkgo

;; Copyright (C) 2014 Gary Slopsema

;; Author: Gary Slopsema <gslopsema@gmail.com>
;; Version: 20140728.1

;; This file is NOT part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(defgroup ginkgo nil
  "Group for configuring ginkgo minor mode"
  :group 'go)

(defcustom ginkgo-use-default-keys nil
  "If true, use the default keybindings"
  :type 'boolean
  :group 'ginkgo
  :safe 'boolean)

(defcustom ginkgo-output-buffer "*ginkgo-output*"
  "Buffer to show ginkgo output"
  :type 'string
  :group 'ginkgo
  :safe 'string)

;; (regexp-opt '("It(" "Context(" "Describe("))
(defcustom ginkgo-containers-regexp "\\(?:\\(?:Context\\|Describe\\|It\\)(\\)"
  "Regexp to recognize ginkgo containers"
  :type 'string
  :group 'ginkgo
  :safe 'string)

(defcustom ginkgo-binary "ginkgo"
  "Name/location of ginkgo binary"
  :type 'string
  :group 'ginkgo
  :safe 'string)

(defvar ginkgo-use-pwd-as-test-dir nil
  "Always use the current working directory as the test directory")

(defvar ginkgo-test-dir ""
  "Location to run gingko tests")

(defvar ginkgo--last-focus ""
    "Holds the description of the last test that was run")

(defun ginkgo--prompt ()
  (read-directory-name "Ginko dir: "))

(defun ginkgo--get-test-dir ()
  (cond
   (ginkgo-use-pwd-as-test-dir default-directory)
   ((string= "" ginkgo-test-dir) (setq ginkgo-test-dir (ginkgo--prompt)))
   (t ginkgo-test-dir)))

;;;###autoload
(defun ginkgo-set-test-dir ()
  "Sets `ginkgo-test-dir'"
  (interactive)
  (setq ginkgo-test-dir (ginkgo--prompt))
  (message "ginkgo-test-dir is %s" ginkgo-test-dir))

(defun ginkgo--run (&rest args)
  (save-selected-window
	(let ((default-directory (concat (ginkgo--get-test-dir) "/"))
		  (arg-string (mapconcat 'identity args " ")))
	  (pop-to-buffer ginkgo-output-buffer)
	  (async-shell-command (format "%s -noisyPendings=false %s" ginkgo-binary arg-string) ginkgo-output-buffer)
	  (message (format "running \"ginkgo %s\" in dir %s" arg-string default-directory)))))

;;;###autoload
(defun ginkgo-run-all ()
  (interactive)
  (ginkgo--run))

;;;###autoload
(defun ginkgo-run-this-container ()
  (interactive)
  (save-excursion
	(while (not (looking-at ginkgo-containers-regexp))
	  (backward-char))
	(let ((start nil)
		  (end nil))
	  (search-forward "\"")
	  (setq start (point))
	  (search-forward "\"")
	  (setq end (- (point) 1))
	  (let ((focus (format "\"%s\""(buffer-substring-no-properties start end))))
		(setq ginkgo--last-focus focus)
		(ginkgo--run "-focus" focus)))))

;;;###autoload
(defun ginkgo-run-last ()
  (interactive)
  (if (string= "" ginkgo--last-focus)
	  (message "No focus string is stored")
	(ginkgo--run "-focus" ginkgo--last-focus)))

;;;###autoload
(defun ginkgo-toggle-pwd-as-test-dir ()
  (interactive)
  (setq ginkgo-use-pwd-as-test-dir (not ginkgo-use-pwd-as-test-dir))
  (ginkgo--update-lighter)
  (message "ginkgo-use-pwd-as-test-dir is %s" ginkgo-use-pwd-as-test-dir))

;;;###autoload
(defun ginkgo-focus-this-container ()
  "Hide everything except this test and the BeforeEach and AfterEach blocks that run as part of this test."
  (interactive)
  (if (not hs-minor-mode)
      (message "hs-minor-mode is not turned on. Turn it on by %s"
               (substitute-command-keys "\\[hs-minor-mode]"))
    (save-excursion
      (let ((location (point)))
        ;; hide all blocks
        (hs-hide-all)
        ;; show one level at a time until the test is visible
        (while (hs-already-hidden-p)
          (hs-show-block)
          (hs-hide-level 1)
          (goto-char location))
        ;; show all BeforeEach & AfterEach blocks that are visible
        (goto-char (point-min))
        (while (search-forward-regexp "^\t+\\(\\(Just\\)?BeforeEach\\|AfterEach\\)")
          (let ((overlay (hs-already-hidden-p)))
            (unless (and overlay
                         (< (overlay-start overlay) (point))
                         (> (overlay-end overlay) (point)))
              (hs-show-block))))))))

;;;###autoload
(defun ginkgo-focus-reset ()
  "Reset the focus and show every test (reverts the effect of 'ginkgo-focus-this-container)."
  (interactive)
  (hs-show-all))

(defun ginkgo--update-lighter ()
  (setcar (cdr (assq 'ginkgo-mode minor-mode-alist)) (ginkgo--lighter)))

(defun ginkgo--lighter ()
  (if ginkgo-use-pwd-as-test-dir
	  " Ginkgo[pwd]"
	" Ginkgo"))

;;;###autoload
(defun ginkgo-bootstrap ()
  (interactive)
  (shell-command "ginkgo bootstrap"))

;;;###autoload
(defun gingko-generate ()
  (interactive)
  (let ((gen-file (file-name-base (buffer-file-name))))
	(shell-command (format "ginkgo generate %s" gen-file))))

(defun ginkgo--make-keymap ()
  (when ginkgo-use-default-keys
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c st") 'ginkgo-set-test-dir)
      (define-key map (kbd "C-c ta") 'ginkgo-run-all)
      (define-key map (kbd "C-c tt") 'ginkgo-run-this-container)
      (define-key map (kbd "C-c tl") 'ginkgo-run-last)
      (define-key map (kbd "C-c tp") 'ginkgo-toggle-pwd-as-test-dir)
      (define-key map (kbd "C-c gg") 'ginkgo-generate)
      (define-key map (kbd "C-c gb") 'ginkgo-bootstrap)
      (define-key map (kbd "C-c ft") 'ginkgo-focus-this-container)
      (define-key map (kbd "C-c fr") 'ginkgo-focus-reset)
      map)))

(define-minor-mode ginkgo-mode
  "Minor mode for ginkgo"
  :lighter (ginkgo--lighter)
  :keymap (ginkgo--make-keymap))

(defun ginkgo-mode-on ()
  (ginkgo-mode 1))

(provide 'ginkgo-mode)
