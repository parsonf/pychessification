# (in-package :com.parsons.chessification)
# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains the actual AI that allows the program to make
# ;;;; good chess moves.
# ;;;;
# ;;;; Author: Brian S Parsons
# ;;;; Last modified: July 2014
# ;;;;
# ;;;; ###########################################################################
#
# ; Give this function a board, a color for who's turn it is, and a ply depth,
# ; and the function will make the move that it determines to be the best move.
# ; It will recursively search through the plies.  As it begins to search a ply,
# ; it will make an educated guess about which move is best by evaluating each move
# ; at that ply.  Then it will evaluate only the moves that fall within a given range
# ; of the supposed-best-move.  The rest that don't fall within range get pruned.
# (defun minimax-depth-first (ll_b color ply)
#         (let ( ; the numbers as the initial values of the variables helped with debugging errors during development. they are arbitrary.
#                 (best-move 61)
#                 (best-node-value 62)
#                 (temp-ll_b 63)
#                 (moves 64)
#                 (what-seems-like-the-best-move 65)
#                 (cutoff-point 66)
#                 (move-value 67)
#                 (current-node-value 68)
#         )
#                 (setf temp-ll_b (copy-board ll_b))
#                 (if (eq ply 0) (progn
#                         (return-from minimax-depth-first (evaluate-game-state temp-ll_b (null color)))
#                 ) (progn
#                         (setf moves (get-all-legal-moves temp-ll_b color *check-check* *check-castle*))
#                         (setf what-seems-like-the-best-move (get-best-score temp-ll_b color moves))
#                         (if (eq color *black*)
#                                 (progn (setf best-node-value 900000) (setf cutoff-point (+ what-seems-like-the-best-move (/ *prune-cutoff* (- (+ 1 *ply*) ply)))))
#                                 (progn (setf best-node-value -900000) (setf cutoff-point (- what-seems-like-the-best-move (/ *prune-cutoff* (- (+ 1 *ply*) ply)))))
#                         )
#                         (if (eq 0 (length moves)) (progn
#                                 (return-from minimax-depth-first (evaluate-game-state temp-ll_b (null color)))
#                         ) (progn
#                                 (loop for move in moves do
#                                         (setf temp-ll_b (copy-board ll_b))
#                                         (board-move temp-ll_b (first move) (second move) (third move) (fourth move) *hypothetical-move*)
#                                         (setf move-value (evaluate-game-state temp-ll_b color))
#                                         (if (eq color *black*) (progn
#                                                 (if (<= move-value cutoff-point) (progn
#                                                         (setf current-node-value (minimax-depth-first temp-ll_b (null color) (- ply 1)))
#                                                         (if (< current-node-value best-node-value) (progn
#                                                                 (setf best-node-value current-node-value)
#                                                                 (setf best-move move)
#                                                         ))
#                                                 ))
#                                         ) (progn
#                                                 (if (>= move-value cutoff-point) (progn
#                                                         (setf current-node-value (minimax-depth-first temp-ll_b (null color) (- ply 1)))
#                                                         (if (> current-node-value best-node-value) (progn
#                                                                 (setf best-node-value current-node-value)
#                                                                 (setf best-move move)
#                                                         ))
#                                                 ))
#                                         ))
#                                 )
#                         ))
#                 ))
#
#                 (if (eq ply *ply*) (progn
#                         (return-from minimax-depth-first best-move)
#                 ) (progn
#                         (return-from minimax-depth-first best-node-value)
#                 ))
#         )
# )
#
# ; evaluates each move of the moves supplied on the given board and returns the best score.
# (defun get-best-score (ll_b color moves)
#         (let (
#                 (temp-ll_b ())
#                 (score 0)
#                 (best-score NIL)
#         )
#                 (if (eq color *black*)
#                         (setf best-score 900000)
#                         (setf best-score -900000)
#                 )
#                 (loop for move in moves do
#                         (setf temp-ll_b (copy-board ll_b))
#                         (board-move temp-ll_b (first move) (second move) (third move) (fourth move) *hypothetical-move*)
#                         (setf score (evaluate-game-state temp-ll_b color))
#                         (if (eq best-score NIL)
#                                 (setf best-score score)
#                         )
#                         (if (eq color *black*)
#                                 (if (> best-score score)
#                                         (setf best-score score)
#                                 )
#                                 (if (< best-score score)
#                                         (setf best-score score)
#                                 )
#                         )
#                 )
#                 (return-from get-best-score best-score)
#         )
# )
#
# ; this function serves as the AI's knowledge-base of opening moves.
# (defun opening-moves (ll_b)
#         (let (
#                 (known-good-opening-moves NIL)
#         )
#                 (if (equal ll_b (list   (list "br" "bn" "bb" "bq" "bk" "bb" "bn" "br")
#                                                                 (list "bp" "bp" "bp" "bp" "bp" "bp" "bp" "bp")
#                                                                 (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                                                 (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                                                 (list "  " "  " "  " "  " "wp" "  " "  " "  ")
#                                                                 (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                                                 (list "wp" "wp" "wp" "wp" "  " "wp" "wp" "wp")
#                                                                 (list "wr" "wn" "wb" "wq" "wk" "wb" "wn" "wr")
#                 ))
#                         (setf known-good-opening-moves (list (list c 7 c 5) ; Sicilian Defense
#                                                                                                  (list e 7 e 6) ; French Defense
#                                                                                                  (list c 7 c 6) ; Caro-Kann Defense
#                                                                                                  (list d 7 d 6) ; Pirc Defense
#                         ))
#                 )
#                 (if (equal ll_b (list   (list "br" "bn" "bb" "bq" "bk" "bb" "bn" "br")
#                                                                 (list "bp" "bp" "bp" "bp" "bp" "bp" "bp" "bp")
#                                                                 (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                                                 (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                                                 (list "  " "  " "  " "wp" "  " "  " "  " "  ")
#                                                                 (list "  " "  " "  " "  " "  " "  " "  " "  ")
#                                                                 (list "wp" "wp" "wp" "  " "wp" "wp" "wp" "wp")
#                                                                 (list "wr" "wn" "wb" "wq" "wk" "wb" "wn" "wr")
#                 ))
#                         (setf known-good-opening-moves (list (list g 8 f 6) ; Indian defense
#                                                                                                  (list d 7 d 5) ; closed game / double queen's pawn opening
#                                                                                                  (list e 7 e 6) ; encouraging white to play e4, entering french defense
#                                                                                                  (list f 7 f 5) ; dutch defense
#                                                                                                  (list d 7 d 6) ; encouraging white to play 34, entering pirc defense
#                         ))
#                 )
#                 (return-from opening-moves known-good-opening-moves)
#         )
#
# )
#
