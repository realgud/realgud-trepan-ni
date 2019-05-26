;;; realgud-trepan-ni.el --- Realgud front-end to trepan-ni -*- lexical-binding: t -*-

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Version: 1.0.0
;; Package-Type: multi
;; Package-Requires: ((load-relative "1.2") (realgud "1.4.8") (cl-lib "0.5") (emacs "25"))
;; URL: http://github.com/realgud/realgud-trepan-ni
;; Compatibility: GNU Emacs 25.x

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

;; realgud support for the "node inspect" with V8 inspector support using a
;; gdb-style "trepan" debugger.
;; See https://nodejs.org/api/debugger.html and https://www.npmjs.com/package/trepan-ni
;;
;;; Code:

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc"))))

(require 'load-relative)

(defgroup realgud-trepan-ni  nil
  "Realgud interface to the 'trepan-ni' debugger"
  :group 'realgud
  :version "25.1")

(require-relative-list '( "./trepan-ni/trepan-ni" ) "realgud-")
(load-relative "./trepan-ni/trepan-ni.el")
(load-relative "./trepan-ni/track-mode.el")

(provide-me)

;;; realgud-trepan-ni.el ends here
