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
      |  |  | |_____| ______| ______| |_____] |_____|    |
      " crlf)
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
; COMPUTERS
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
    (slot powered-on (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot disk-size (default nil))
    (slot disk-type (allowed-values ssd mechanical unknown) (default unknown))
    (slot ram-size (default nil))
    (slot built-in-display (type SYMBOL) (allowed-symbols yes no unknown) (default unknown)) ; built-in, monitor, TODO not sure how to represent
    (slot external-displays (type INTEGER) (default 0))
    (multislot network-source (allowed-values wifi ethernet unknown) (default unknown))
)



(deftemplate COMPUTERS::problem
    (slot off-and-on-again (type SYMBOL) (allowed-symbols yes no) (default no))
    (slot is-crashing (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot wont-turn-on (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot no-internet (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot screen-blank (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot error-message (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
)

;Create a blank computer for us to troubleshoot
(deffacts COMPUTERS::the-computer
    (computer)
    (problem)
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
    ?prob <- (problem
        (off-and-on-again no)
    )
    =>
    (modify ?prob
        (off-and-on-again (get-answer "HAVE YOU TRIED TURNING IT OFF AND ON AGAIN?" (create$ yes no)))
    )
)

(defrule QUESTIONS::wont-turn-on
    ?prob <- (problem
        (off-and-on-again yes)
        (wont-turn-on unknown)
    )
    =>
    (if (eq (get-answer "IS YOUR COMPUTER ABLE TO TURN ON?" (create$ yes no)) yes)
        then (modify ?prob (wont-turn-on no))
        else (modify ?prob (wont-turn-on yes))
    )
)

(defrule QUESTIONS::plugged-in
    ?pc <- (computer
        (plugged-in unknown)
    )
    ?prob <- (problem
        (off-and-on-again yes)
        (wont-turn-on yes)
    )
    =>
    (modify ?pc
        (plugged-in (get-answer "IS YOUR COMPUTER PLUGGED IN?" (create$ yes no)))
    )
)


; (defrule QUESTIONS::powered-on
;     ?pc <- (computer
;         (powered-on unknown)
;     )
;     =>
;     (modify ?pc
;         (powered-on (get-answer "IS YOUR COMPUTER POWERED ON?" (create$ yes no)))
;     )
; )

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
        else (modify ?pc
            (form-factor desktop)
            (built-in-display no)
            (power-source outlet)
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
