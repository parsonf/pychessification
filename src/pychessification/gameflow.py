# (in-package :com.parsons.chessification)
# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains functions that assist with game flow for the chess game.
# ;;;;
# ;;;; Author: Brian S Parsons
# ;;;; Last modified: July 2014
# ;;;;
# ;;;; ###########################################################################
#
# ; this is the function called that starts the game.
# ; for all intents and purposes, it is the "main" method.
# (defun run-chess ()
#         (let (
#                 (cb (new-game))
#         )
#                 (format t "The game has begun.~%~%")
#                 (format t "Type \"help\" to see what input is accepted.~%")
#                 (format t "Type \"end\" to end the game.~%")
#                 (format t "If you already know what input is expected, you may begin entering moves.~%")
#                 (chess-round cb)
#         )
# )
#
# ; continuously loops through chess rounds until the user decides to quit.
# (defun chess-round (cb)
#         (let (
#                 (moved NIL)
#                 (done "end")
#                 (help "help")
#                 (moves "moves")
#                 (reset "reset")
#                 (input "derp")
#                 (my-move NIL)
#         )
#                 (print-board cb)
#                 (loop while T do
#                         (setf input "derp")
#                         (loop until (or (valid-chess-move input) (equal reset input) (equal done input) (equal help input) (equal moves input)) do
#                                 (setf input (read-line))
#                         )
#                         (setf moved NIL)
#                         (if (equal done input) (progn
#                                 (format t "Chess program has ended. Type (run-chess) to start a new game.~%~%")
#                                 (return-from chess-round NIL)
#                         ))
#                         (if (equal help input)
#                                 (display-help)
#                         )
#                         (if (equal reset input) (progn
#                                 (setf cb (new-game))
#                         ))
#                         (if (equal moves input)
#                                 (print-moves (get-all-legal-moves cb *white* *check-check* *check-castle*))
#                         )
#                         (if (valid-chess-move input)
#                                 (if (member (parse-chess-move input)
#                                                         (get-all-legal-moves cb *white* *check-check* *check-castle*)
#                                                         :test #'equal) (progn
#                                         (setf my-move (parse-chess-move input))
#                                         (board-move (copy-list cb) (first my-move) (second my-move) (third my-move) (fourth my-move) *actual-move*)
#                                         (setf moved T)
#                                 ))
#                         )
#                         (if moved (progn
#                                 (print-board cb)
#                                 (let (
#                                         (game-state (evaluate-game-state cb *black*))
#                                 )
#                                         (format t "Good move.  State of the game is ~A.  Hmm.... let me think...." game-state)
#                                 )
#                                 ;(sleep 3)
#                                 (let ((ai-move NIL) (game-state 247) (moves-left 46))
#                                         (setf ai-move (ai-turn cb))
#                                         (if (equal ai-move "you beat me") (progn
#                                                 (format t "~%~%That's checkmate.  You win.  Good game.~%~%")
#                                                 (return-from chess-round)
#                                         ) (progn
#                                                 (print-board cb)
#                                                 (print-move ai-move)
#                                                 (setf game-state (evaluate-game-state cb *black*))
#                                                 (format t " State of the game is ~A.~%" game-state)
#                                                 (setf moves-left (length (get-all-legal-moves cb *white* *check-check* *check-castle*)))
#                                                 (if (eq moves-left 0) (progn
#                                                         (format t "~%~%  That's checkmate....I beat you.... I BEAT YOU !! HAHA !! I WIN !!!! WOOOO !!!! ~%~%")
#                                                         (format t "~%~%  .... I mean.... good game....")
#                                                         (return-from chess-round)
#                                                 ))
#                                         ))
#                                 )
#                         ) (progn
#                                 (print-board cb)
#                         ))
#                 )
#         )
# )
#
# ; the events that transpire when it is the AI's turn to move.
# (defun ai-turn (ll_b)
#         (let (
#                 (rnd-opening 0)
#                 (openings (opening-moves ll_b))
#                 (moves-left 38)
#                 (best-move ())
#         )
#                 ; if the board is a known opening move.
#                 (if (null (null openings)) (progn
#                         ; let's give it some variety for a bit of variation.
#                         (setf rnd-opening (nth (random (length openings)) openings))
#                         (board-move ll_b (first rnd-opening) (second rnd-opening) (third rnd-opening) (fourth rnd-opening) *actual-move*)
#                 ) (progn
#                         (setf moves-left (length (get-all-legal-moves ll_b *black* *check-check* *check-castle*)))
#                         (if (> moves-left 0) (progn
#                                 (setf best-move (minimax-depth-first ll_b *black* *ply*))
#                                 (board-move ll_b (first best-move) (second best-move) (third best-move) (fourth best-move) *actual-move*)
#                                 (return-from ai-turn best-move)
#                         ) (progn
#                                 (return-from ai-turn "you beat me")
#                         ))
#                 ))
#         )
#
# )
#
# ; returns a fresh board for a new chess game to be played.
# (defun new-game ()
#         (setf *wk-has-moved* NIL)
#         (setf *bk-has-moved* NIL)
#         (setf *qs-wr-has-moved* NIL)
#         (setf *ks-wr-has-moved* NIL)
#         (setf *qs-br-has-moved* NIL)
#         (setf *ks-br-has-moved* NIL)
#         (copy-list (list (list "br" "bn" "bb" "bq" "bk" "bb" "bn" "br")
#                                          (list "bp" "bp" "bp" "bp" "bp" "bp" "bp" "bp")
#                                          (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                          (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                          (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                          (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                          (list "wp" "wp" "wp" "wp" "wp" "wp" "wp" "wp")
#                                          (list "wr" "wn" "wb" "wq" "wk" "wb" "wn" "wr")
#         ))
# )
#
# ; this function does not validate whether a user-submitted
# ; move is a legal move, but rather, it validates whether
# ; what was supplied was the user was in the format expected.
# ; the format expected is "[a-h][1-8][a-h][1-8]".
# ; once this is validated, it will be treated as a chess move
# ; at some later point, and checked to see if the move is an
# ; available one to take.
# (defun valid-chess-move (input)
#         (let (
#                 (validp NIL)
#                 (from-row NIL)
#                 (from-col NIL)
#                 (dest-row NIL)
#                 (dest-col NIL)
#         )
#                 ; length is four
#                 (if (eq 4 (length input)) (progn
#                         (setf from-row (subseq input 0 1))
#                         (setf from-col (subseq input 1 2))
#                         (setf dest-row (subseq input 2 3))
#                         (setf dest-col (subseq input 3 4))
#                         (if (and (member from-row '("a" "b" "c" "d" "e" "f" "g" "h") :test #'equal)
#                                          (member from-col '("1" "2" "3" "4" "5" "6" "7" "8") :test #'equal)
#                                          (member dest-row '("a" "b" "c" "d" "e" "f" "g" "h") :test #'equal)
#                                          (member dest-col '("1" "2" "3" "4" "5" "6" "7" "8") :test #'equal))
#                                 (setf validp T)
#                         )
#                 ))
#
#                 (return-from valid-chess-move validp)
#         )
# )
#
# ; at this point, we know that what was submitted
# ; is a valid chess move, so we are going to parse
# ; it as such and return a move list.
# (defun parse-chess-move (input)
#         (let (
#                 (from-row (subseq input 0 1))
#                 (from-col (subseq input 1 2))
#                 (dest-row (subseq input 2 3))
#                 (dest-col (subseq input 3 4))
#         )
#                 (setf from-row  (if (equal from-row "a") a (if (equal from-row "b") b
#                                                 (if (equal from-row "c") c (if (equal from-row "d") d
#                                                 (if (equal from-row "e") e (if (equal from-row "f") f
#                                                 (if (equal from-row "g") g (if (equal from-row "h") h NIL)))))))))
#                 (setf from-col  (if (equal from-col "1") 1 (if (equal from-col "2") 2
#                                                 (if (equal from-col "3") 3 (if (equal from-col "4") 4
#                                                 (if (equal from-col "5") 5 (if (equal from-col "6") 6
#                                                 (if (equal from-col "7") 7 (if (equal from-col "8") 8 NIL)))))))))
#                 (setf dest-row  (if (equal dest-row "a") a (if (equal dest-row "b") b
#                                                 (if (equal dest-row "c") c (if (equal dest-row "d") d
#                                                 (if (equal dest-row "e") e (if (equal dest-row "f") f
#                                                 (if (equal dest-row "g") g (if (equal dest-row "h") h NIL)))))))))
#                 (setf dest-col  (if (equal dest-col "1") 1 (if (equal dest-col "2") 2
#                                                 (if (equal dest-col "3") 3 (if (equal dest-col "4") 4
#                                                 (if (equal dest-col "5") 5 (if (equal dest-col "6") 6
#                                                 (if (equal dest-col "7") 7 (if (equal dest-col "8") 8 NIL)))))))))
#
#                 (return-from parse-chess-move (list from-row from-col dest-row dest-col))
#         )
# )
