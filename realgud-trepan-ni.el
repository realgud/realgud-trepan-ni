;;; realgud-trepan-ni.el --- realgud front-end to newer "node inspect"

;; Author: Rocky Bernstein
;; Version: 1.0.0
;; Package-Type: multi
;; Package-Requires: ((realgud "1.4.5") (cl-lib "0.5") (emacs "24"))
;; URL: http://github.com/realgud/realgud-node-inspect
;; Compatibility: GNU Emacs 24.x

;; Copyright (C) 2015, 2016, 2019 Free Software Foundation, Inc

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

;;; Commentary:

;; realgud support for the "node inspect" with V8 inspector support.
;; See https://nodejs.org/api/debugger.html
;;
;;; Code:

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc"))))n

(require 'load-relative)

(defgroup realgud-trepan-ni  nil
  "Realgud interface to the 'trepan-ni' debugger"
  :group 'realgud
  :version "24.3")

(require-relative-list '( "./trepan-ni/main" ) "realgud-")
(load-relative "./trepan-ni/main.el")
(load-relative "./trepan-ni/track-mode.el")

(provide-me)

;;; realgud-trepan-ni.el ends here
