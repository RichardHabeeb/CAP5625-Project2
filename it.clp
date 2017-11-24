;/-------------------------------------------------------------------------------\
; Expert System for IT Repair
;
;       Course: CAP 5625 - Introduction to AI
;       Authors: Anwesh Tuladhar, Richard Habeeb
;
;       CLIPS Version 6.3
;       To execute: (load it.clp)
;                   (reset)
;                   (run)
;\-------------------------------------------------------------------------------/

(defmodule MAIN (export ?ALL))

;#################################################################################
; FUNCTION DEFS
;#################################################################################


;#################################################################################
; TEMPLATES
;#################################################################################
(defmodule COMPUTERS (import MAIN ?ALL))

(deftemplate COMPUTERS::operating-system
    (slot kind (allowed-values windows osx linux other) (default ?NONE)))

(deftemplate COMPUTERS::computer
    (slot brand (default ?NONE))
    (slot form-factor (default ?NONE)) ; desktop, laptop, server
    (multislot os (type SYMBOL) (default ?NONE))
    (slot power-source (allowed-values battery outlet) (default ?DERIVE))
    (slot disk-size (default ?NONE))
    (slot disk-type (allowed-values ssd mechanical) (default ?NONE))
    (slot built-in-display (type SYMBOL) (allowed-symbols yes no) (default ?DERIVE)) ; built-in, monitor, TODO not sure how to represent
    (slot external-displays (type INTEGER) (default 0))
    (multislot network-source (allowed-values wifi ethernet) (default ?DERIVE)))





;#################################################################################
; QUESTIONS
;#################################################################################




;#################################################################################
; EXPERT RULES
;#################################################################################
