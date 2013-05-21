;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; testel.el                                                                 ;; 
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

(when (null (get 'test-failed 'error-conditions))
  (put 'test-failed 'error-conditions '(testel-conditions test-failed))
  (put 'test-failed 'error-message "Test Failed"))

(defun test-t (condition msg)
	(when (not condition)
		(signal 'test-failed msg)))

(defun test-equal (a b msg)
  (when (not (equal a b))
    (signal 'test-failed msg)))

(defun test-string= (str1 str2 msg)
  (when (not (string= str1 str2))
    (signal 'test-failed msg)))

(defun test= (val1 val2 msg)
	(when (not (= val1 val2))
		(signal 'test-failed msg)))

(defcustom default-output-buffer (get-buffer "*Messages*")
	"Default output location for defunittest"
	:type 'buffer)		

(defmacro defunittest (test-name &rest body)
  `(defun ,test-name (&optional output-buffer)
     ,(if (stringp (car body)) ;; Quick test to see if a doc string was included
					(car body)
				"")
		 (interactive)
     ,(let ((passed-count (gensym))
            (failed-count (gensym))
            (error-count (gensym))
            (test (gensym))
            (err (gensym)))
        `(with-current-buffer (if (or (null output-buffer) (not (bufferp output-buffer)))
                                  default-output-buffer
                                output-buffer)
             (let ((,passed-count 0)
                   (,failed-count 0)
                   (,error-count 0))
               ,@(mapcar #'(lambda (test) 
                             `(condition-case ,err
                                  (progn
                                    ,@(cdr test)
                                    (incf ,passed-count)
                                    (insert (format "%s : Passed\n" ,(first test))))
                                (test-failed                             
                                 (incf ,failed-count)
                                 (insert (format "%s : Failed \n\t%s\n" ,(first test) (cdr ,err))))
                                (error 
                                 (incf ,error-count)
                                 (insert (format "%s : Error\n\t%s\n" ,(first test) (second ,err))))))
                         (if (stringp (car body)) (cdr body) body))
               (list ,passed-count ,failed-count ,error-count))))))