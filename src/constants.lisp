(in-package #:json-reader)

(declaim (character +colon+))
(defconstant +colon+ #\:
  "The character for the colon.")

(declaim (character +comma+))
(defconstant +comma+ #\,
  "The character for the comma.")

(declaim (character +left-brace+))
(defconstant +left-brace+ #\{
  "The character for the left brace.")

(declaim (character +left-bracket+))
(defconstant +left-bracket+ #\[
  "The character for the left bracket.")

(declaim (character +right-brace+))
(defconstant +right-brace+ #\}
  "The character for the right brace.")

(declaim (character +right-bracket+))
(defconstant +right-bracket+ #\]
  "The character fo the right bracket.")
