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


(deffunction print-moss ()
    (printout t "

                                ,&&&&&
                               &&&&&&&&%
    ,,,,                     &&&&&&&&&&%                ,,,
  ,,,,,,,         ///////////&&&&&&&&&&&&//           ,,,,,,,
    ,,,,,       *&&&&&&&&&&&&&&&&&&&&&&&&&&           ,,,,,
    ,,,,        *&&&&&&&&&&&&&&&&&&&&&&&&&&&&           ,,,
    ,,,,        *&&&&&&&&,,,,,,,/&&&&&&&&&&&&           ,,,
    ,,,,.         &&&&&&&,,,,,,,,,((((&&&&&((         ..,,,
      ,,,             &&&&&&&&&&&&&&&&&&&             ,,,
      ,,,              .&      &(     &%              ,,,
      ,,,              .&&&&&&&,/&&&&&&%              ,,,
      ,,,                ,,,,,,,,,,,,,                ,,,
      ,,,,,              ,,,,,,,,,,,,,              ,,,,,
        ,,,,,            ,,,,,,,,,,,,,            ,,,,,,
         ,,,,,,            ,,,,,,,,,             ,,,,,
           ,,,,,.            ,,,,,             ,,,,,
               ,.              &(              ,.


                               ..
                               &(
      _______  _____  _______ _______ ______   _____  _______
      |  |  | |     | |______ |______ |_____] |     |    |
      |  |  | |_____| ______| ______| |_____] |_____|    |     " crlf)
)

(defrule MAIN::start
    =>
    (printout t "=============================================================" crlf)
    (print-moss)
    (printout t "=============================================================" crlf)
    (printout t "MOSSBOT: ACTIVATED." crlf)
    (printout t "MOSSBOT: READY TO TROUBLESHOOT YOUR COMPUTER PROBLEM." crlf)
    (printout t "=============================================================" crlf)
    (focus QUESTIONS)
    )

;#################################################################################
; TEMPLATES
;#################################################################################
(defmodule COMPUTERS (import MAIN ?ALL) (export ?ALL))

(deftemplate COMPUTERS::operating-system
    (slot kind (allowed-values windows osx linux other) (default ?NONE)))

(deftemplate COMPUTERS::computer
    (slot brand (default ?DERIVE))
    (slot form-factor (allowed-values laptop desktop unknown) (default unknown))
    (multislot os (default nil))
    (slot power-source (allowed-values battery outlet unknown) (default unknown))
    (slot plugged-in (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot disk-size (default ?DERIVE))
    (slot disk-type (allowed-values ssd mechanical) (default ?DERIVE))
    (slot built-in-display (type SYMBOL) (allowed-symbols yes no unknown) (default unknown)) ; built-in, monitor, TODO not sure how to represent
    (slot external-displays (type INTEGER) (default 0))
    (multislot network-source (allowed-values wifi ethernet) (default ?DERIVE))
    (slot off-and-on-again (type SYMBOL) (allowed-symbols yes no) (default no))
)

;Create a blank computer for us to troubleshoot
(deffacts COMPUTERS::the-computer
    (computer)
)


;#################################################################################
; QUESTIONS
;#################################################################################
(defmodule QUESTIONS (import COMPUTERS ?ALL))



(deffunction QUESTIONS::ask (?text ?allowed-answers)

    (printout t "=============================================================" crlf)
    (printout t "MOSSBOT: " ?text crlf ?allowed-answers "> ")
    (bind ?ans (read)) ; Read keyboard input
    (if (stringp ?ans) then ;if answer is a string
        (bind ?ans (lowcase ?ans)) ;convert to lowercase
    )
    (printout t "=============================================================" crlf)
    ?ans
)

(deffunction QUESTIONS::get-answer (?text ?allowed-answers)
    (bind ?ans (ask ?text ?allowed-answers))

    (while (not (member ?ans ?allowed-answers))
        (printout t "Error with input: " ?ans crlf)
        (bind ?ans (ask ?text ?allowed-answers))
    )
    ?ans
)

;this is a really simple way that doesn't require parsing question objects -> use pattern matching
(defrule QUESTIONS::off-and-on-again
    ?pc <- (computer
        (off-and-on-again no)
    )
    =>
    (modify ?pc
        (off-and-on-again (get-answer "HAVE YOU TRIED TURNING IT OFF AND ON AGAIN?" (create$ yes no)))
    )
)

(defrule QUESTIONS::is-laptop
    ?pc <- (computer
        (form-factor unknown)
        (built-in-display ?display)
        (power-source ?power)
    )
    =>
    (if (eq (get-answer "IS YOUR COMPUTER A LAPTOP?" (create$ yes no)) yes)
        then (modify ?pc
            (form-factor laptop)
            (built-in-display yes)
            (power-source battery)
        )
    )
)


; (deftemplate QUESTIONS::question
;     (slot text (default ?NONE))
;     (multislot allowed-answers (default ?NONE))
;     (slot asked (type SYMBOL) (allowed-symbols yes no) (default no))
;     ;(slot attribute (default ?NONE))
;     (slot result (default ?DERIVE))
; )

; (defrule QUESTIONS::ask
;     ?q <- (question
;         (asked no)
;         (text ?t)
;         (allowed-answers $?valid)
;     )
;     ?pc <- (computer
;     )
;     =>
;     (printout t ?t " ")
;     (printout t ?valid crlf)
;     ;(assert)
;     (modify ?q (asked yes) (result (read)))
; )
;
; (deffacts QUESTIONS::expert-questions
;     (question
;         (text "Is your computer a laptop?")
;         (allowed-answers yes no)
;     )
;     (question
;         (text "Have you tried turning it off and on again?")
;         (allowed-answers yes no)
;     )
; )
