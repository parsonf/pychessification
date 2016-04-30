# (in-package :com.parsons.chessification)
# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains functions relevant to printing information.
# ;;;;
# ;;;; Author: Brian S Parsons
# ;;;; Last modified: July 2014
# ;;;;
# ;;;; ###########################################################################
#
# ; this function displays help to the screen.
# (defun display-help ()
#         (format t "Commands:~%")
#         (format t "moves       - Displays your available moves.~%")
#         (format t "help        - Displays this help message.~%")
#         (format t "reset       - Starts a new game of chess.~%")
#         (format t "end         - Ends the chess program.~%")
#         (format t "How to Move:~%")
#         (format t "Type the row and column of the piece to be moved,~%")
#         (format t "followed by the row and column of the space to be moved to.~%")
#         (format t "Here are some examples of moves:~%")
#         (format t "e2e4     (pawn to e4)~%")
#         (format t "g1f3     (knight to f3)~%")
#         (format t "For castle moves, type the space of the king followed by the space where~%")
#         (format t "the king will end up.  For example:~%")
#         (format t "e1g1     (white castle king-side)~%")
#         (format t "e1c1     (white castle queen-side)~%")
#         (format t "~%")
#         (format t "Other than that, enjoy the game!~%~%")
# )
#
# ; this function prints the given board to the screen
# ; in the most text-friendly way that I could muster.
# (defun print-board (board)
#         (format t "~%")
#         (format t "    ##################################################~%")
#         (loop :for y :from 0 :to 7 :do
#                 (format t "    ")
#                 (if (oddp y)
#                         (format t "#+----+      +----+      +----+      +----+      #~%")
#                         (format t "#      +----+      +----+      +----+      +----+#~%")
#                 )
#                 (format t " ~A  #" (- 8 y))
#                 (loop :for x :from 0 :to 7 :do
#                         (if (oddp (mod (+ x y) 2))
#                                 (format t "| ~A |" (nth x (nth y board)))
#                                 (format t "  ~A  " (nth x (nth y board)))
#                         )
#                 )
#                 (format t "#~%    ")
#                 (if (oddp y)
#                         (format t "#+----+      +----+      +----+      +----+      #~%")
#                         (format t "#      +----+      +----+      +----+      +----+#~%")
#                 )
#         )
#         (format t "    ##################################################~%")
#         (format t "~%")
#         (format t "        a     b     c     d     e     f     g     h  ~%")
#         (format t "~%")
# )
#
# ; prints a list of your available moves.
# (defun print-moves (moves)
#                 (loop for move in moves do
#                         (print-move move)
#                         (format t "~%")
#                 )
# )
#
# ; prints a given move to the screen.
# (defun print-move (move)
#         (let (
#                 (rowp T)
#                 (row NIL)
#         )
#                 (loop for pos in move do
#                         (if rowp
#                                 (progn
#                                         (setf row (case pos (0 "a") (1 "b") (2 "c") (3 "d") (4 "e") (5 "f") (6 "g") (7 "h")))
#                                         (format t "~A" row)
#                                 )
#                                 ;else, column.
#                                 (format t "~A" pos)
#                         )
#                         (setf rowp (null rowp))
#                 )
#         )
# )
