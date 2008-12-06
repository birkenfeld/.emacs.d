;;; python+rst.el --- Jinja mode highlighting
;;
;; Author: Georg Brandl
;; Last modified: 2008-05-22 23:22 by gbr
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Mostly ripped off django-mode by Lennart Borgman.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'mumamo)
(require 'rst)

(defun mumamo-chunk-tqs (pos min max)
;  (mumamo-quick-static-chunk pos min max "\"\"\"" "\"\"\"" t 'rst-mode)
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-tqs
                              'mumamo-search-bw-exc-end-tqs
                              'mumamo-search-fw-exc-start-tqs
                              'mumamo-search-fw-exc-end-tqs))

(defun mumamo-search-bw-exc-start-tqs (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "\"\"\"")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'rst-mode))))

(defun mumamo-search-bw-exc-end-tqs (pos min)
  (mumamo-chunk-end-bw-str pos min "\"\"\""))

(defun mumamo-search-fw-exc-start-tqs (pos max)
  (mumamo-chunk-start-fw-str-inc pos max "\"\"\""))

(defun mumamo-search-fw-exc-end-tqs (pos max)
  (mumamo-chunk-end-fw-str pos max "\"\"\""))


;;;###autoload
(define-mumamo-multi-major-mode python-rst-mumamo
  "Multiple major mode setup for Python with reST docstrings."
  ("Python with reST docstrings" python-mode
   (mumamo-chunk-tqs)))

(provide 'python+rst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python+rst.el ends here
