;/-------------------------------------------------------------------------------\
; Expert System for IT Repair
;
;       Course: CAP 5625 - Introduction to AI
;       Authors: Anwesh Tuladhar, Richard Habeeb
;
;       CLIPS Version 6.3
;       To execute: (load it_cls.clp)
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

(defmessage-handler USER init before ()
(printout t "*** Creating an instance ***" crlf))

(defmessage-handler USER init after ()
(printout t "*** Done creating an instance ***" crlf))

(deftemplate COMPUTERS::OS
    (slot kind (allowed-values windows osx linux other) (default ?NONE))
    (slot has-run-troubleshooter (allowed-values yes no) (default no))
)

; (defclass COMPUTERS::OS
;     (is-a USER)
;     (role abstract)
;     (slot kind (allowed-values windows osx linux other) (default ?NONE))
; )

; (defclass COMPUTERS::WINDOWS
;     (is-a OS)
;     (role concrete)
;     (slot has-run-troubleshooter (allowed-values yes no) (default no))
; )

; (defclass COMPUTERS::OSX
;     (is-a OS)
;     (role concrete)
;     (slot has-run-network-diagnostics (allowed-values yes no) (default no))
; )

; (defclass COMPUTERS::LINUX
;     (is-a OS)
;     (role concrete)
;     (slot has-run-troubleshooter (allowed-values yes no) (default no))
; )

(defclass COMPUTERS::COMPUTER
    (is-a USER)
    (slot brand (default ?DERIVE))
    (slot form-factor (allowed-values laptop desktop unknown) (default unknown))
    ; (multislot os (default nil))
    ; (slot os (allowed-values windows osx other unknown) (default unknown))
    (slot os )
    (slot has-run-troubleshooter (allowed-values yes no) (default no))
    (slot power-source (allowed-values battery outlet unknown) (default unknown))
    (slot plugged-in (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot powered-on (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot disk-size (default nil))
    (slot disk-type (allowed-values ssd mechanical unknown) (default unknown))
    (slot ram-size (default nil))
    (slot built-in-display (type SYMBOL) (allowed-symbols yes no unknown) (default unknown)) ; built-in, monitor, TODO not sure how to represent
    (slot external-displays (type INTEGER) (default 0))
    (slot network-source (allowed-values wifi ethernet unknown none) (default unknown))

    (slot has-cleared-history (allowed-values yes no unknown) (default unknown))
    (slot browser (allowed-values firefox chrome other unknown) (default unknown))

    ; (multislot network-source (allowed-values wifi ethernet unknown) (default unknown))
)

(defclass COMPUTERS::NETWORK
    (is-a USER)
    (slot router-has-internet (allowed-values yes no unknown) (default unknown))
    (slot is-in-range (allowed-values yes no unknown) (default unknown))
    (slot is-ethernet-plugged-in (allowed-values yes no unknown) (default unknown))
    (slot has-been-restarted (allowed-values yes no) (default no))
    (slot no-network-issue (allowed-values true false) (default false))
)


(defclass COMPUTERS::PROBLEM
    (is-a USER)
    (slot off-and-on-again (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    ; (slot is-crashing (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot wont-turn-on (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot no-internet (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot slow-internet (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot screen-blank (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
    (slot error-message (type SYMBOL) (allowed-symbols yes no unknown) (default unknown))
)

;Create a blank computer for us to troubleshoot
; (deffacts COMPUTERS::the-computer
;     (computer)
;     (problem)
; )
; (definstance COMPUTERS::the-computer
;     ()
; )

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

(deffunction QUESTIONS::diag-step (?step)
    (printout t "-------------------------------------------------------------" crlf)
    (printout t "MOSSBOT INSTRUCTION: " ?step crlf)
    (printout t "-------------------------------------------------------------" crlf)
)

(deffunction QUESTIONS::final-answer (?answer)
    (printout t "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" crlf)
    (printout t "MOSSBOT SOLUTION: " ?answer crlf)
    (printout t "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" crlf)
)


;#################################################################################
; RULES
;#################################################################################

; 1. Figure out what type of computer first.
(defrule QUESTIONS::form-factor
    (declare (salience 1000))
    =>
    ; read input from user
    (bind ?form (get-answer "Is your device a laptop or a desktop or unknown?" (create$ laptop desktop unknown)))
    (if (eq ?form laptop)
     then (make-instance [laptop] of COMPUTER
             (form-factor laptop)
             (power-source battery)
          )
     else (if (eq ?form desktop)
           then (make-instance [desktop] of COMPUTER
                    (form-factor desktop)
                    (power-source outlet)
                )
           else (make-instance [unknown] of COMPUTER
                    (form-factor unknown)
                )
          )
    )
    (make-instance [problem] of PROBLEM)
)

;this is a really simple way that doesn't require parsing question objects -> use pattern matching
;this is also a reference to IT crowd:
(defrule QUESTIONS::off-and-on-again
    ?comp <- (object (is-a COMPUTER))
    ?prob <- (object (is-a PROBLEM) 
                (off-and-on-again unknown)
             )
    =>
    (send ?prob put-off-and-on-again (get-answer "HAVE YOU TRIED TURNING IT OFF AND ON AGAIN?" (create$ yes no)))
)

(defrule QUESTIONS::off-and-on-again-1
    ?comp <- (object (is-a COMPUTER))
    ?prob <- (object (is-a PROBLEM) 
                (off-and-on-again no)
             )
    =>
    (final-answer "Please turn it off and on again.")
)

(defrule QUESTIONS::what-are-the-problems
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                ; (is-crashing unknown)
                (wont-turn-on unknown)
                (no-internet unknown)
                (slow-internet unknown)
             )
    =>
    (bind ?ison (get-answer "Does your computer power on?" (create$ yes no)))
    (send ?prob put-wont-turn-on ?ison)
    (if (eq ?ison yes)
     then 
        (bind ?no-internet (get-answer "Are you having problems connecting to the internet?" (create$ yes no)))
        (send ?prob put-no-internet ?no-internet)
        (if (eq ?no-internet no)
            then 
            (bind ?slow-internet (get-answer "Are you having problems with slow internet?" (create$ yes no)))
            (send ?prob put-slow-internet ?slow-internet)
            (if (eq ?slow-internet no)
             then 
                (final-answer "Have a nice day!")
            )
        ) 
    )
)

;#################################################################################
; RULES FOR WONT TURN ON
;#################################################################################
(defrule QUESTIONS::wont-turn-on-desktop-1
    (declare (salience 500))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (wont-turn-on no)
             )
    ?desktop <- (object (is-a COMPUTER)
                    (form-factor desktop)
                    (plugged-in unknown)
                )
    =>
    (if (eq (get-answer "Is the power-source plugged-in?" (create$ yes no)) no)
        then 
            (send ?desktop put-plugged-in no)
            (final-answer "Please plug-in the power-source.")
        else 
            (send ?desktop put-plugged-in yes)
            (final-answer "Please replace your power-source.")
    )
)

(defrule QUESTIONS::wont-turn-on-laptop-1
    (declare (salience 500))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (wont-turn-on no)
             )
    ?laptop <- (object (is-a COMPUTER)
                    (form-factor laptop)
                    (plugged-in unknown)
                )
    =>
    ; (bind ?charged (get-answer "Is your battery fully charged?" (create$ yes no unknown)))
    (bind ?charged (get-answer "Is your battery fully charged?" (create$ yes no)))
    (if (eq ?charged no)
     then 
        (send ?laptop put-plugged-in no)
        (diag-step "Please plug-in the power-source to charge your battery.")
     else (send ?laptop put-plugged-in no)
     ; else (if (eq ?charged unknown)
     ;       then (send ?laptop put-plugged-in no)
     ;      )
     )
)

(defrule QUESTIONS::wont-turn-on-laptop-2
    (declare (salience 500))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (wont-turn-on no)
             )
    ?laptop <- (object (is-a COMPUTER)
                    (form-factor laptop)
                    (plugged-in no)
               )
    =>
    (if (eq (get-answer "Is the power-source plugged-in?" (create$ yes no)) no)
        then 
            (send ?laptop put-plugged-in no)
            (diag-step "Please plug-in the power-source.")
        else 
            (send ?laptop put-plugged-in yes)
            (final-answer "Please replace your battery.")
    )
)

;#################################################################################
; RULES FOR NO INTERNET
;#################################################################################
; (defrule QUESTIONS::no-internet-1
;     (declare (salience 400))
;     ?prob <- (object (is-a PROBLEM)
;                 (off-and-on-again yes)
;                 (no-internet yes)
;              )
;     ?comp <- (object (is-a COMPUTER)
;                     (network-source unknown)
;                )
;     =>
;     (bind ?nic (get-answer "Are you using wifi or ethernet?" (create$ wifi ethernet none)))
;     (send ?comp put-network-source ?nic)
;     (make-instance [network] of NETWORK)
;     (if (eq  ?nic wifi)
;         then 
;             (send ?comp put-network-source wifi)
;         else (if (eq ?nic ethernet) 
;               then (send ?comp put-network-source ethernet)
;               else (send ?comp put-network-source none)
;              )
;     )
; )

(defrule QUESTIONS::no-internet-1
    (declare (salience 400))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (no-internet ?no-internet)
                (slow-internet ?slow-internet)
             )
    (test (or (eq ?no-internet yes)
              (eq ?slow-internet yes)
          )
    )
    ?comp <- (object (is-a COMPUTER)
                    (network-source unknown)
               )
    =>
    (bind ?nic (get-answer "Are you using wifi or ethernet?" (create$ wifi ethernet none)))
    (send ?comp put-network-source ?nic)
    (make-instance [network] of NETWORK)
    (if (eq  ?nic wifi)
        then 
            (send ?comp put-network-source wifi)
        else (if (eq ?nic ethernet) 
              then (send ?comp put-network-source ethernet)
              else (send ?comp put-network-source none)
             )
    )
)

(defrule QUESTIONS::no-internet-network-1
    (declare (salience 300))
    ?net <- (object (is-a NETWORK)
            )
    =>
    (if (eq  (get-answer "Does the router have internet (Check LED labelled internet)?" (create$ yes no)) yes)
        then 
            (send ?net put-router-has-internet yes)
        else 
            (send ?net put-router-has-internet no)
            ; (final-answer "Please contact the internet service provider.")
    )
    ; (assert ())
)

(defrule QUESTIONS::no-internet-network-2
    (declare (salience 300))
    ?net <- (object (is-a NETWORK)
                    (router-has-internet yes)
            )
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (no-internet yes)
             )
    ?comp <- (object (is-a COMPUTER)
                     (network-source wifi)
             )
    =>
    (if (eq  (get-answer "Is the router in range?" (create$ yes no)) yes)
        then 
            (send ?net put-is-in-range yes)
        else 
            (send ?net put-is-in-range no)
            (final-answer "Please get closer to the router.")
    )
)

(defrule QUESTIONS::no-internet-network-3
    (declare (salience 300))
    ?net <- (object (is-a NETWORK)
                    (router-has-internet yes)
            )
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (no-internet yes)
             )
    ?comp <- (object (is-a COMPUTER)
                     (network-source ethernet)
             )
    =>
    (if (eq  (get-answer "Is the computer connected to the router with an ethernet cable?" (create$ yes no)) yes)
        then 
            (send ?net put-is-ethernet-plugged-in yes)
        else 
            (send ?net put-is-ethernet-plugged-in no)
            (final-answer "Please connected your computer to the router with an ethernet cable.")
    )
) 

(defrule QUESTIONS::no-internet-network-4
    (declare (salience 300))
    ?net <- (object (is-a NETWORK)
                    (router-has-internet yes)
                    (is-ethernet-plugged-in ?ethernet)
                    (is-in-range ?range)    
            )
    (test (or (eq ?ethernet yes) 
              (eq ?range yes)
          )
    )
    =>
    (send ?net put-no-network-issue true)
)

(defrule QUESTIONS::no-internet-network-5
    (declare (salience 250))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (slow-internet ?slow-internet)
             )
    ?net <- (object (is-a NETWORK)
                    (router-has-internet ?has-internet)
                    (has-been-restarted no)
            )
    (test (or (eq ?slow-internet yes)
              (eq ?has-internet no)
          )
    )
    =>
    (diag-step "Please restart your router to resolve the issue.")
    (assert (router-restart))
)

(defrule QUESTIONS::no-internet-network-6
    (declare (salience 350))
    ?diag <- (router-restart)
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (no-internet ?no-internet)
                (slow-internet ?slow-internet)
             )
    ?net <- (object (is-a NETWORK)
                    ; (router-has-internet no)
                    ; (has-been-restarted no)
            )
    =>
    (retract ?diag)
    (send ?net put-has-been-restarted yes)
    (if (eq (get-answer "Was the issue fixed after the restart?" (create$ yes no) ) yes)
     then 
        (send ?net put-router-has-internet yes)
        (final-answer "Issue Resolved. Thank you for visiting.")
     else 
        (if (eq ?no-internet yes)
         then 
            (send ?net put-router-has-internet no)
            (final-answer "Please contact the internet service provider.")
        )
        ; (if (eq ?slow-internet yes)
        ;  then 
        ;     (send ?comp put-router-has-internet yes)
        ; )

    )
)

(defrule QUESTIONS::no-internet-2
    (declare (salience 400))
    ?net <- (object (is-a NETWORK)
                    (no-network-issue true)
            )
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (no-internet yes)
             )
     ?comp <- (object (is-a COMPUTER)
                     (network-source ?ns)
             )
     (test (or (eq ?ns ethernet)
               (eq ?ns wifi)
           )
     )
    =>
    (send ?comp put-os (get-answer "What operating system are you using?" (create$ windows osx)))
)

; Windows internet issues.
(defrule QUESTIONS::no-internet-windows-1
    (declare (salience 400))
    ?prob <- (object (is-a PROBLEM)
                     (off-and-on-again yes)
                     (no-internet yes)
             )

    ?comp <- (object (is-a COMPUTER)
                     (os windows)
                     (has-run-troubleshooter no)
             )
    =>
    (diag-step "Please run the troubleshooter to help resolve the problem.")
    (diag-step "(1. Click on the searchbox on the taskbar.)")
    (diag-step "(2. Search for Network troublesooter.)")
    (assert (windows-diagnostics))
)

(defrule QUESTIONS::no-internet-windows-2
    (declare (salience 400))
    ?diag <- (windows-diagnostics)
    ?comp <- (object (is-a COMPUTER)
                     (os windows)
             )
    =>
    (retract ?diag)
    (if (eq (get-answer "Was the issue fixed by the troubleshooter?" (create$ yes no) ) yes)
     then (final-answer "Issue Resolved. Thank you for visiting.")
     else (send ?comp put-has-run-troubleshooter yes)
    )
)

(defrule QUESTIONS::no-internet-windows-3
    (declare (salience 400))
     ?comp <- (object (is-a COMPUTER)
                     (os windows)
                     (has-run-troubleshooter yes)
             )
    =>
    (final-answer "Possible solutions are:")    
    (final-answer "1. Temporarily turn off firewalls, antivirus software and/or malware-prevention software.")
    (final-answer "2. Update/reinstall network driver.")
)

; OSX internet issues.
(defrule QUESTIONS::no-internet-osx-1
    (declare (salience 400))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (no-internet yes)
             )
    ?comp <- (object (is-a COMPUTER)
                     (os osx)
                     (has-run-troubleshooter no)
             )
    =>
    (diag-step "Please run the network diagnostics to help resolve the problem.")
    (diag-step "1. Click Apple menu > System Preferences > Network > Assist Me > Diagnostics")
    (diag-step "2. Follow on screen instructions.")
    (assert (osx-diagnostics))
)

(defrule QUESTIONS::no-internet-osx-2
    (declare (salience 400))
    ?diag <- (osx-diagnostics)
    ?comp <- (object (is-a COMPUTER)
                     (os osx)
             )
    =>
    (retract ?diag)
    (if (eq (get-answer "Did the network diagnostics resolve your issue?" (create$ yes no) ) yes)
     then (final-answer "Issue Resolved. Thank you for visiting.")
     else (send ?comp put-has-run-troubleshooter yes)
    )
)

(defrule QUESTIONS::no-internet-osx-3
    (declare (salience 400))
    ?comp <- (object (is-a COMPUTER)
                     (os osx)
                     (has-run-troubleshooter yes)
             )
    =>
    (final-answer "Possible solutions are:")    
    (final-answer "1. Compare your network preferences/DNS settingswith those recommended by the ISP.")
    (final-answer "2. Turn off/on your network connection services.")
)

;#################################################################################
; RULES FOR SLOW INTERNET
;#################################################################################
(defrule QUESTIONS::slow-internet-0
    ; (declare (salience 400))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (slow-internet yes)
             )
    ?net <- (object (is-a NETWORK)
                    (router-has-internet no)
                    (has-been-restarted yes)
            )
    ?comp <- (object (is-a COMPUTER)
             )
    =>
    (final-answer "Please contact the internet service provider.")
)

(defrule QUESTIONS::slow-internet-1
    ; (declare (salience 400))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (slow-internet yes)
             )
    ?net <- (object (is-a NETWORK)
                    (router-has-internet yes)
                    (has-been-restarted yes)
            )
    ?comp <- (object (is-a COMPUTER)
             )
    =>
    (send ?comp put-has-cleared-history (get-answer "Have you tried clearing the browser history?" (create$ yes no) ))
)

(defrule QUESTIONS::slow-internet-2
    ; (declare (salience 400))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (slow-internet yes)
             )
    ?comp <- (object (is-a COMPUTER)
                     (has-cleared-history yes)
             )
    =>
    (send ?comp put-browser (get-answer "Which browser are you using?" (create$ chrome firefox other)))
)

(defrule QUESTIONS::slow-internet-3
    ; (declare (salience 400))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (slow-internet yes)
             )
    ?comp <- (object (is-a COMPUTER)
                     (has-cleared-history no)
             )
    =>
    (diag-step "Please clear browser history.")
    (assert (cleared-history))
)

(defrule QUESTIONS::slow-internet-4
    ?diag <- (cleared-history)
    ?comp <- (object (is-a COMPUTER)
                     (has-cleared-history no)
             )
    =>
    (retract ?diag)
    (if (eq (get-answer "Did it help?" (create$ yes no)) yes) 
     then (final-answer "Have a nice day.")    
     else (send ?comp put-has-cleared-history yes)
    )
)

(defrule QUESTIONS::slow-internet-5
    ; (declare (salience 400))
    ?prob <- (object (is-a PROBLEM)
                (off-and-on-again yes)
                (slow-internet yes)
             )
    ?comp <- (object (is-a COMPUTER)
                     (browser ?browser)
             )
    (test (not (eq ?browser unknown)))
    =>
    (diag-step "Try using just a single tab in a single browser.")
    (assert (single-tab))
)

(defrule QUESTIONS::slow-internet-6
    ; (declare (salience 400))
    ?diag <- (single-tab)
    ?comp <- (object (is-a COMPUTER)
                     (browser ?browser)
             )
    =>
    (retract ?diag)
    (if (or (eq ?browser chrome)
            (eq ?browser firefox)
        )
     then 
        (diag-step "Please disable all the extensions/addons.")
        (assert (disabled-extension))
     else (final-answer "Please use firefox or chrome browser.")
    )
)

(defrule QUESTIONS::slow-internet-7
    ?diag <- (disabled-extension)
    =>
    (retract ?diag)
    (if (eq (get-answer "Did it help?" (create$ yes no)) yes) 
     then (final-answer "Have a nice day.")    
     else 
        (final-answer "Possible solutions are:")    
        (final-answer "1. Scan computer for adware/malware/virus using antivirus software.")
        (final-answer "2. Contact your ISP for better internet service.")    
    )
)

; (defrule QUESTIONS::slow-internet-6
;     ; (declare (salience 400))
;     ?diag <- (disabled-extension)
;     =>
;     (retract ?diag)
;     (final-answer "Possible solutions are:")    
;     (final-answer "1. Scan computer for adware/malware/virus using antivirus software.")
;     (final-answer "2. Contact your ISP for better internet service.")    
; )
; (defrule QUESTIONS::is-crashing
;     ?prob <- (problem
;         (off-and-on-again yes)
;         (wont-turn-on no)
;         (screen-blank no)
;         (is-crashing unknown)
;     )
;     =>
;     (modify ?prob
;         (is-crashing (get-answer "IS YOUR COMPUTER CRASHING?" (create$ yes no)))
;     )
; )


; (defrule QUESTIONS::wont-turn-on
;     ?prob <- (problem
;         (off-and-on-again yes)
;         (wont-turn-on unknown)
;     )
;     =>
;     (if (eq (get-answer "IS YOUR COMPUTER ABLE TO TURN ON?" (create$ yes no)) yes)
;         then (modify ?prob (wont-turn-on no))
;         else (modify ?prob (wont-turn-on yes))
;     )
; )


; (defrule QUESTIONS::internet-working
;     ?prob <- (problem
;         (off-and-on-again yes)
;         (wont-turn-on no)
;         (screen-blank no)
;         (no-internet unknown)
;     )
;     =>
;     (if (eq (get-answer "IS YOUR INTERNET CONNECTION WORKING?" (create$ yes no)) yes)
;         then (modify ?prob (no-internet no))
;         else (modify ?prob (no-internet yes))
;     )
; )


; (defrule QUESTIONS::screen-blank
;     ?prob <- (problem
;         (off-and-on-again yes)
;         (wont-turn-on no)
;         (screen-blank unknown)
;     )
;     =>
;     (if (eq (get-answer "IS YOUR SCREEN WORKING?" (create$ yes no)) yes)
;         then (modify ?prob (screen-blank no))
;         else (modify ?prob (screen-blank yes))
;     )
; )

; (defrule QUESTIONS::error-message
;     ?prob <- (problem
;         (off-and-on-again yes)
;         (wont-turn-on no)
;         (screen-blank no)
;         (error-message unknown)
;     )
;     =>
;     (modify ?prob
;         (error-message (get-answer "ARE YOU SEEING AN ERROR MESSAGE?" (create$ yes no)))
;     )
; )


; (defrule QUESTIONS::plugged-in
;     ?pc <- (computer
;         (plugged-in unknown)
;     )
;     ?prob <- (problem
;         (off-and-on-again yes)
;         (wont-turn-on yes)
;     )
;     =>
;     (modify ?pc
;         (plugged-in (get-answer "IS YOUR COMPUTER PLUGGED IN?" (create$ yes no)))
;     )
; )


; (defrule QUESTIONS::is-laptop
;     ?pc <- (computer
;         (form-factor unknown)
;         (built-in-display ?display)
;         (power-source ?power)
;     )
;     =>
;     (if (eq (get-answer "IS YOUR COMPUTER A LAPTOP?" (create$ yes no)) yes)
;         then (modify ?pc
;             (form-factor laptop)
;             (built-in-display yes)
;             (power-source battery)
;         )
;         else (modify ?pc
;             (form-factor desktop)
;             (built-in-display no)
;             (power-source outlet)
;         )
;     )
; )


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
