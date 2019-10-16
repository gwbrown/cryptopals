(ql:quickload '(:arrow-macros :s-base64))
(use-package :arrow-macros)

(defvar *s1c1-hex* "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(defvar *s1c1-base64* "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

(defun bytes->string (bytes)
  "Takes a sequence of integers represnting ascii chars and returns a string"
  (map 'string #'code-char bytes))

(defun maybe-pad-string (str)
  "Ensures a string is of even length by pre-padding with 0,
   so that it can be split into pairs"
  (if (/= 0 (mod (length str) 2))
      (concatenate 'string "0" str)
      str))

(defun string->pairs (str)
  "Splits a string into pairs of characters"
  (let ((str (maybe-pad-string str)))
    (loop for i upto (- (length str) 2) by 2
          collect (subseq str i (+ i 2)))))

(defun hexpairs->bytes (hexpairs)
  "Takes pairs of characters (as strings) and parses them as hexadecimal"
  (map 'vector #'(lambda (pair) (parse-integer pair :radix 16)) hexpairs))

(defun hex->bytes (hexstr)
  "Takes a hex string and parses it into character codes"
  (-> hexstr
    string->pairs
    hexpairs->bytes))

(defun hex->string (hexstr)
  "A convenience function to convert a hex string directly into a regular string"
  (-> hexstr
    hex->bytes
    bytes->string))

(defun bytes->base64 (bytes)
  "Converts a sequence of bytes to a string"
  (with-output-to-string (out)
    (s-base64:encode-base64-bytes bytes out)))

(defun base64->bytes (str)
  (with-input-from-string (in str)
    (s-base64:decode-base64-bytes in)))

(defun check-s1c1 ()
  (equalp *s1c1-base64* (-> *s1c1-hex* hex->bytes bytes->base64)))
