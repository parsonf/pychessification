# (in-package :com.parsons.chessification)
# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains functions relevant to the board.
# ;;;;
# ;;;; Author: Brian S Parsons
# ;;;; Last modified: July 2014
# ;;;;
# ;;;; ###########################################################################
#
# ; gets the piece at the given position.
# (defun cb-at (ll_b x y)
#         (nth x (nth (- 8 y) ll_b))
# )
#
# ; moves whatever is at the originating space to the destination space,
# ; and clears the originating space to a vacant one.
# ; checks if the move is a castling move. if so, it moves the rook too.
# ; checks if the move is a pawn promotion move. if so, it is promoted to a queen.
# (defun board-move (ll_b fx fy tx ty actual-movep)
#         (let (
#                 (piece-to-move (cb-at ll_b fx fy))
#         )
#                 ; keeping castle checks valid.
#                 (if actual-movep (progn
#                         (if (equal piece-to-move "wk") (setf *wk-has-moved* T))
#                         (if (equal piece-to-move "bk") (setf *bk-has-moved* T))
#                         (if (and (equal piece-to-move "wr") (eq fx a) (eq fy 1)) (setf *qs-wr-has-moved* T))
#                         (if (and (equal piece-to-move "wr") (eq fx h) (eq fy 1)) (setf *ks-wr-has-moved* T))
#                         (if (and (equal piece-to-move "br") (eq fx a) (eq fy 8)) (setf *qs-br-has-moved* T))
#                         (if (and (equal piece-to-move "br") (eq fx h) (eq fy 8)) (setf *ks-br-has-moved* T))
#                 ))
#         )
#         (setf (nth tx (nth (- 8 ty) ll_b)) (nth fx (nth (- 8 fy) ll_b)))
#         (setf (nth fx (nth (- 8 fy) ll_b)) (vacant))
#
#         ; if castling, move the rook, too.
#         (if (and (eq fx e) (eq fy 1) (eq tx g) (eq ty 1)) (progn ; white castle king side
#                 (setf (nth f (nth (- 8 1) ll_b)) (nth h (nth (- 8 1) ll_b)))
#                 (setf (nth h (nth (- 8 1) ll_b)) (vacant))
#         ) (if (and (eq fx e) (eq fy 1) (eq tx c) (eq ty 1)) (progn ; white castle queen side
#                 (setf (nth d (nth (- 8 1) ll_b)) (nth a (nth (- 8 1) ll_b)))
#                 (setf (nth a (nth (- 8 1) ll_b)) (vacant))
#         ) (if (and (eq fx e) (eq fy 8) (eq tx g) (eq ty 8)) (progn ; black castle king side
#                 (setf (nth f (nth (- 8 8) ll_b)) (nth h (nth (- 8 8) ll_b)))
#                 (setf (nth h (nth (- 8 8) ll_b)) (vacant))
#         ) (if (and (eq fx e) (eq fy 8) (eq tx c) (eq ty 8)) (progn ; black castle queen side
#                 (setf (nth d (nth (- 8 8) ll_b)) (nth a (nth (- 8 8) ll_b)))
#                 (setf (nth a (nth (- 8 8) ll_b)) (vacant))
#         )))))
#
#         ; if a white pawn reaching the 8th row, promote to queen.
#         (if (and (equal "wp" (cb-at ll_b tx ty)) (eq ty 8))
#                 (setf (nth tx (nth (- 8 ty) ll_b)) "wq")
#         )
#         ; if a black pawn reaching the 1st row, promote to queen.
#         (if (and (equal "bp" (cb-at ll_b tx ty)) (eq ty 1))
#                 (setf (nth tx (nth (- 8 ty) ll_b)) "bq")
#         )
# )
#
# ; determines if the given space is occupied by a friendly piece.
# (defun friendly-occupiedp (ll_b x y my-color)
#         (let (
#                 (piece (cb-at ll_b x y))
#                 (piece-color (get-piece-color (cb-at ll_b x y)))
#         )
#                 ;if the space is vacant, false.
#                 (if (equal (vacant) piece)
#                         NIL
#                         ;if the space is occupied by enemy piece, false.
#                         (if (null (eq my-color piece-color))
#                                 NIL
#                                 ;if the space is occupied by your piece, true.
#                                 (if (eq my-color piece-color)
#                                         T
#                                         NIL
#                                 )
#                         )
#                 )
#         )
# )
#
# ; determines if the given space is occupied by an enemy piece.
# (defun enemy-occupiedp (ll_b x y my-color)
#         (let (
#                 (piece (cb-at ll_b x y))
#                 (piece-color (get-piece-color (cb-at ll_b x y)))
#         )
#                 ;if the space is vacant, false.
#                 (if (equal (vacant) piece)
#                         NIL
#                         ;if the space is occupied by your piece, false.
#                         (if (eq my-color piece-color)
#                                 NIL
#                                 ;if the space is occupied by enemy piece, true.
#                                 (if (null (eq my-color piece-color))
#                                         T
#                                         NIL
#                                 )
#                         )
#                 )
#         )
# )
#
# ; makes a copy of the given board.
# (defun copy-board (ll_b)
#         (let (
#                 (nb ())
#                 (nr ())
#         )
#                 (loop for row in ll_b do
#                         (setf nr ())
#                         (loop for space in row do
#                                 (setf nr (append nr (list space)))
#                         )
#                         (setf nb (append nb (list nr)))
#                 )
#
#                 (return-from copy-board nb)
#         )
# )
#
# ; determines if the given space is even on the board.
# (defun valid-spacep (x y)
#         (and (<= x h)
#                  (>= x a)
#                  (<= y 8)
#                  (>= y 1)
#         )
# )
