# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains global variables for the chess program.
# ;;;;
# ;;;; Author: Brian S Parsons
# ;;;; Last modified: July 2014
# ;;;;
# ;;;; ###########################################################################
#
# (defpackage :com.parsons.chessification
#   (:use :common-lisp :common-lisp-user :cl :common-graphics :common-graphics-user :cg :excl))
#
# (in-package :com.parsons.chessification)
#
# ;;;;;tweaking minimax algorithm
# ;------------------------------------
#         ;how many plies to search
#         (defvar *ply* 4)
#         ;range within supposed-best-move's score to decide to evaluate move or prune the branch.
#         (defvar *prune-cutoff* 120)
#
# ;;;;;helping readability
# ;------------------------------------
#         ; rows
#         (defvar a 0)
#         (defvar b 1)
#         (defvar c 2)
#         (defvar d 3)
#         (defvar e 4)
#         (defvar f 5)
#         (defvar g 6)
#         (defvar h 7)
#
#         ; colors of the pieces
#         (defvar *white* T)
#         (defvar *black* NIL)
#
#         ; checking for check
#         (defvar *check-check* T)
#         (defvar *ignore-check* NIL)
#
#         ; check castling
#         (defvar *check-castle* T)
#         (defvar *ignore-castle* NIL)
#
#         ; for castling
#         (defvar *wk-has-moved* NIL)
#         (defvar *bk-has-moved* NIL)
#         (defvar *qs-wr-has-moved* NIL)
#         (defvar *ks-wr-has-moved* NIL)
#         (defvar *qs-br-has-moved* NIL)
#         (defvar *ks-br-has-moved* NIL)
#
#         ; is a move an actual move, or just a hypothetical move. matters for castling rules..
#         (defvar *actual-move* T)
#         (defvar *hypothetical-move* NIL)
#
#
