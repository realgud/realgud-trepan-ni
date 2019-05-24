;;; track-mode.el ---
;; Copyright (C) 2015, 2016-2019 Free Software Foundation, Inc
;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; "trepan-ni" tracking a comint buffer.

(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-track-mode-setup   'realgud-track-mode)
(declare-function realgud:remove-ansi-schmutz 'realgud:utils)

(require 'realgud)

(require-relative-list '("core" "init") "realgud:trepan-ni-")

(realgud-track-mode-vars "realgud:trepan-ni")

;; FIXME: this shouldn't be needed
(defvar realgud:trepan-ni-track-mode-map (make-keymap))
(define-key realgud:trepan-ni-track-mode-map
  (kbd "C-c !f") 'realgud:js-goto-file-line)
(define-key realgud:trepan-ni-track-mode-map
  (kbd "C-c !s") 'realgud:js-goto-syntax-error-line)

(defun realgud:trepan-ni-track-mode-hook()
  (if realgud:trepan-ni-track-mode
      (progn
	(use-local-map trepanjs-track-mode-map)
	(realgud:remove-ansi-schmutz)
	(message "using trepan-ni mode map")
	)
    (message "trepan-ni track-mode-hook disable called"))
)
(define-minor-mode realgud:trepan-ni-track-mode
  "Minor mode for tracking trepan-ni source locations inside a trepan-ni shell via realgud.

If called interactively with no prefix argument, the mode is
toggled. A prefix argument, captured as ARG, enables the mode if
the argument is positive, and disables it otherwise.

\\{realgud:trepan-ni-track-mode-map}"
  :init-value nil
  ;; :lighter " trepan-ni"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:trepan-ni
  :keymap realgud:trepan-ni-track-mode-map

  (if realgud:trepan-ni-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (realgud:trepan-ni-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
  )

;; ;; Debugger commands that trepan-ni doesn't have
;; (define-key trepan-ni-track-mode-map
;;   [remap realgud:cmd-newer-frame] 'undefined)
;; (define-key trepan-ni-track-mode-map
;;   [remap realgud:cmd-older-frame] 'undefined)
(defvar realgud:trepan-ni-short-key-mode-map (make-keymap))

(define-key realgud:trepan-ni-short-key-mode-map
  [remap realgud:cmd-step] 'realgud:cmd-step-no-arg)
(define-key realgud:trepan-ni-short-key-mode-map
  [remap realgud:cmd-step] 'realgud:cmd-step-no-arg)
(define-key realgud:trepan-ni-short-key-mode-map
  [remap realgud:cmd-next] 'realgud:cmd-next-no-arg)

(provide-me "realgud:trepan-ni-")
