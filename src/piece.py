# (in-package :com.parsons.chessification)
# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains functions relevant to pieces on the board.
# ;;;;
# ;;;; Author: Brian S Parsons
# ;;;; Last modified: July 2014
# ;;;;
# ;;;; ###########################################################################
#
# ; gets the color of a piece, given the string of a piece.
# (defun get-piece-color (piece-string)
#         (if (member piece-string
#         '("wp" "wr" "wn" "wb" "wq" "wk")
#         :test #'equal)
#                 *white*
#                 (if (member piece-string
#                 '("bp" "br" "bn" "bb" "bq" "bk")
#                 :test #'equal)
#                         *black*
#                         'error-piece-neither-black-nor-white
#                 )
#         )
# )
#
# ; just a convenience function.
# (defun vacant ()
#         "  "
# )
