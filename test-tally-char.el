;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; test-tally-char.el                                                        ;; 
;; Copyright (c) 2013 Thomas Hartman (rokstar83@gmail.com)                   ;; 
;;                                                                           ;; 
;; This program is free software; you can redistribute it and/or             ;; 
;; modify it under the terms of the GNU General Public License               ;; 
;; as published by the Free Software Foundation; either version 2            ;; 
;; of the License, or the License, or (at your option) any later             ;; 
;; version.                                                                  ;; 
;;                                                                           ;; 
;; This program is distributed in the hope that it will be useful,           ;; 
;; but WITHOUT ANY WARRANTY; without even the implied warranty of            ;; 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             ;; 
;; GNU General Public License for more details.                              ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defvar *test-tally-char-buffer* (get-buffer-create "*Tally Char Tests*"))

(defvar test-tally-char-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "r" 'run-tally-char-tests)
    map))

(defgroup test-tally-char nil
	"Support for tally-char testing."
	:group 'tools
	:prefix  "test-tally-char")

(define-derived-mode test-tally-char-mode fundamental-mode "tally-char Testing"
  "Tally-char testing major mode
\\{test-tally-char-mode-map}"
  :group 'test-tally-char
  (use-local-map test-tally-char-mode-map))

(put 'pm-test-mode 'mode-class 'special)

(defun start-tally-char-tests ()
	(interactive)
	(set-buffer *test-tally-char-buffer*)
	(switch-to-buffer *test-tally-char-buffer*)
	(test-tally-char-mode)
	(run-tally-char-tests))

(defun run-tally-char-tests ()
	(interactive)
	(with-current-buffer *test-tally-char-buffer*
		(kill-region (point-min) (point-max)) ; Delete the old results, if any
		(insert "tally-char unit tests:\n\n")
		(test-tally-char *test-tally-char-buffer*)))

(require 'testel)

(defunittest test-tally-char
  "Unit test for tally-char"
  ("Test tallying"
   (test= (tally-char "erase" ?e) 2 "There should be 2 e's in 'erase'!")
	 (test= (tally-char "zero" ?z) 1 "There should be 1 z in 'zero'!")
	 (test= (tally-char "noodle" ?p) 0 "There should be 0 p's in 'noodle'!"))
	("Test bad parameters"
   (test-t (null (tally-char 'foo ?p)) "Should return nil with symbol passed as string.")
	 (test-t (null (tally-char "foo" 'o)) "Should return nil with symbol passed as character.")
	 (test-t (null (tally-char 'foo 'bar)) "Should return nil with two symbols passed as parameters.")))

;; This normally would be in its own seperate source file but for demonstration purposes i've 
;; kept it with it's unit testing framework

(defun tally-char (str c)
  "Count and return the number of characters 'c' in the string str. Returns nil if wrong parameters
	 are provided."
	(if (or (not (integerp c)) (not (stringp str)))
  	  nil
    (loop for x across str count (= x c))))
