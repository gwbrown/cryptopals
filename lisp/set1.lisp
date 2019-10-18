(ql:quickload '(:arrow-macros :s-base64 :bit-smasher))
(use-package :arrow-macros)
(use-package :bit-smasher)

(defvar *s1c1-hex* "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(defvar *s1c1-base64* "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

(defun octets->string (bytes)
  "Takes a sequence of integers represnting ascii chars and returns a string"
  (map 'string #'code-char bytes))

(defun maybe-pad-string (str)
  "Ensures a string is of even length by pre-padding with 0,
   so that it can be split into pairs"
  (if (/= 0 (mod (length str) 2))
      (concatenate 'string "0" str)
      str))

(defun hex->string (hexstr)
  "A convenience function to convert a hex string directly into a regular string"
  (-> hexstr
    hex->octets
    octets->string))

(defun octets->base64 (octets)
  "Converts a sequence of bytes to a string"
  (with-output-to-string (out)
    (s-base64:encode-base64-bytes octets out)))

(defun base64->octets (str)
  (with-input-from-string (in str)
    (s-base64:decode-base64-bytes in)))

(defun check-s1c1 ()
  (equalp *s1c1-base64* (-> *s1c1-hex* hex->octets octets->base64)))

(defvar *s1c2-input* "1c0111001f010100061a024b53535009181c")
(defvar *s1c2-key* "686974207468652062756c6c277320657965")
(defvar *s1c2-answer* "746865206b696420646f6e277420706c6179")

(defun check-s1c2 ()
  (equalp *s1c2-answer* (bits->hex (bit-xor (hex->bits *s1c2-input*) (hex->bits *s1c2-key*)))))
