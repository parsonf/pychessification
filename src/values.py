# (in-package :com.parsons.chessification)
# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains all the information available
# ;;;; that assists the AI in determining whether the state
# ;;;; of a game is "good" or "bad" for a given player (black or white).
# ;;;;
# ;;;; Author: Brian S Parsons
# ;;;; Last modified: July 2014
# ;;;;
# ;;;; ###########################################################################
#
# ; base values of pieces
# (defvar *pawn-value* 100)
# (defvar *rook-value* 500)
# (defvar *knight-value* 300)
# (defvar *bishop-value* 300)
# (defvar *queen-value* 1000)
# (defvar *king-value* 900)
#
# ; I see no value in encouraging or discouraging number of pawn moves.
# (defvar *pawn-move-value* 0)
# ; rooks are very useful when they control entire columns. number of moves is very valuable.
# (defvar *rook-move-value* 50)
# ; knights are most useful when they have moves. forks are awesome. knights are very mobile in tight spots. yes yes.
# (defvar *knight-move-value* 60)
# ; bishops are most useful when they control diagonals.
# (defvar *bishop-move-value* 50)
# ; queens are rooks plus bishops... but... don't want to be encouraging risking the queen too much.
# (defvar *queen-move-value* 20)
# ; kings... they do best in hiding, but when being checked, best to have available moves.
# ; so, neither encourage nor discourage, i guess.
# (defvar *king-move-value* 0)
#
#
# ; the next several list of lists assign values
# ; to certain pieces occupying certain spaces on the board.
# ; they are assigned from white's perspective, but when black
# ; evaluates, the board is flipped and reversed.
# (defvar *pawn-position-value* (list
#         (list  90  90  90  90  90  90  90  90)
#         (list  50  50  50  50  50  50  50  50)
#         (list  10  10  20  30  30  20  10  10)
#         (list   5   5  10  25  25  10   5   5)
#         (list   0   0   0  20  20   0   0   0)
#         (list   5  -5  -10  0   0 -10  -5   5)
#         (list   5  10  10 -20 -20  10  10   5)
#         (list   0   0   0   0   0   0   0   0)
# ))
#
# (defvar *knight-position-value* (list
#         (list -50 -40 -30 -30 -30 -30 -40 -50)
#         (list -40 -20   0   0   0   0 -20 -40)
#         (list -30   0  10  15  15  10   0 -30)
#         (list -30   5  15  20  20  15   5 -30)
#         (list -30   0  15  20  20  15   0 -30)
#         (list -30   5  10  15  15  10   5 -30)
#         (list -40 -20   0   5   5   0 -20 -40)
#         (list -50 -40 -30 -30 -30 -30 -40 -50)
# ))
#
# (defvar *bishop-position-value* (list
#         (list -20 -10 -10 -10 -10 -10 -10 -20)
#         (list -10   0   0   0   0   0   0 -10)
#         (list -10   0   5  10  10   5   0 -10)
#         (list -10   5   5  10  10   5   5 -10)
#         (list -10   0  10  10  10  10   0 -10)
#         (list -10  10  10  10  10  10  10 -10)
#         (list -10   5   0   0   0   0   5 -10)
#         (list -20 -10 -10 -10 -10 -10 -10 -20)
# ))
#
# (defvar *rook-position-value* (list
#         (list   0   0   0   0   0   0   0   0)
#         (list   5  10  10  10  10  10  10   5)
#         (list  -5   0   0   0   0   0   0  -5)
#         (list  -5   0   0   0   0   0   0  -5)
#         (list  -5   0   0   0   0   0   0  -5)
#         (list  -5   0   0   0   0   0   0  -5)
#         (list  -5   0   0   0   0   0   0  -5)
#         (list   0   0   0   5   5   0   0   0)
# ))
#
# (defvar *queen-position-value* (list
#         (list -20 -10 -10  -5  -5 -10 -10 -20)
#         (list -10   0   0   0   0   0   0 -10)
#         (list -10   0   5   5   5   5   0 -10)
#         (list  -5   0   5   5   5   5   0  -5)
#         (list   0   0   5   5   5   5   0 -10)
#         (list -10   0   5   5   5   5   0 -10)
#         (list -10   0   5   0   0   0   0 -10)
#         (list -20 -10 -10  -5  -5 -10 -10 -20)
# ))
#
# (defvar *king-position-value* (list
#         (list -30 -40 -40 -50 -50 -40 -40 -30)
#         (list -30 -40 -40 -50 -50 -40 -40 -30)
#         (list -30 -40 -40 -50 -50 -40 -40 -30)
#         (list -30 -40 -40 -50 -50 -40 -40 -30)
#         (list -20 -30 -30 -40 -40 -30 -30 -20)
#         (list -10 -20 -20 -20 -20 -20 -20 -10)
#         (list  20  20   0   0   0   0  20  20)
#         (list  20  30  10   0   0  10  30  20)
# ))
#
# ; we flip the board vertically and horizontally for black so
# ; that black can properly evaluate its position on the board.
# (defun ppv-white (x y) (nth x (nth (- 8 y) *pawn-position-value*)))
# (defun ppv-black (x y) (nth (- 7 x) (nth (- 7 (- 8 y)) *pawn-position-value*)))
# (defun rpv-white (x y) (nth x (nth (- 8 y) *rook-position-value*)))
# (defun rpv-black (x y) (nth (- 7 x) (nth (- 7 (- 8 y)) *rook-position-value*)))
# (defun npv-white (x y) (nth x (nth (- 8 y) *knight-position-value*)))
# (defun npv-black (x y) (nth (- 7 x) (nth (- 7 (- 8 y)) *knight-position-value*)))
# (defun bpv-white (x y) (nth x (nth (- 8 y) *bishop-position-value*)))
# (defun bpv-black (x y) (nth (- 7 x) (nth (- 7 (- 8 y)) *bishop-position-value*)))
# (defun qpv-white (x y) (nth x (nth (- 8 y) *queen-position-value*)))
# (defun qpv-black (x y) (nth (- 7 x) (nth (- 7 (- 8 y)) *queen-position-value*)))
# (defun kpv-white (x y) (nth x (nth (- 8 y) *king-position-value*)))
# (defun kpv-black (x y) (nth (- 7 x) (nth (- 7 (- 8 y)) *king-position-value*)))
#
# ; the next few functions return the value for the piece (given in the function name)
# ; of a certain color on a certain spot.
# (defun p-pos-val (x y color)
#         (if (eq color *white*)
#                 (ppv-white x y)
#                 (ppv-black x y)
#         )
# )
# (defun r-pos-val (x y color)
#         (if (eq color *white*)
#                 (rpv-white x y)
#                 (rpv-black x y)
#         )
# )
# (defun n-pos-val (x y color)
#         (if (eq color *white*)
#                 (npv-white x y)
#                 (npv-black x y)
#         )
# )
# (defun b-pos-val (x y color)
#         (if (eq color *white*)
#                 (bpv-white x y)
#                 (bpv-black x y)
#         )
# )
# (defun q-pos-val (x y color)
#         (if (eq color *white*)
#                 (qpv-white x y)
#                 (qpv-black x y)
#         )
# )
# (defun k-pos-val (x y color)
#         (if (eq color *white*)
#                 (kpv-white x y)
#                 (kpv-black x y)
#         )
# )
#
# ; Here is where the magic happens.
# ; This function is provided a board and a color.
# ; The color indicates who's move we are evaluating.
# ; So if the color given is black, we are evaluating black's move (although we don't know what the move was).
# ; Knowing who's move we are evaluating allows us to look at who's turn it would be, which in turn
# ; allows us to put a lesser value on threatened pieces.
# ; As a result, the same board would return different scores based on who's move we are evaluating,
# ; because the difference is who is about to make a move on this board.
# ; White favors a strong positive number.  If the result of the function is strongly positive,
# ; that indicates a favorable state for white.  If the result of the function is strongly negative,
# ; that indicates a favorable state for black.
# (defun evaluate-game-state (ll_b color)
#         (let (
#                 (value 0)
#                 (minep NIL)
#                 (piece NIL)
#                 (piece-color NIL)
#                 (threatenedp 284)
#         )
#                 ; encourage checkmate, discourage stalemate...
#                 (if (eq 0 (length (get-all-legal-moves ll_b (null color) *check-check* *check-castle*)))
#                         ;if opponent is in check, then it is checkmate! value REALLY HIGH
#                         ;however, if not, then it is stalemate! value REALLY LOW
#                         (if (in-checkp ll_b (null color))
#                                 (setf value (+ value 90000)) ; DO IT !!!!!!!!!!
#                                 (setf value (- value 90000)) ; DONT DO IT !!!!!!!!!!!!!
#                         )
#                 )
#                 (loop :for y :from 1 :to 8 :do
#                         (loop :for x :from a :to h :do
#                                 (setf piece (cb-at ll_b x y))
#                                 (setf piece-color (get-piece-color piece))
#                                 ;if this is a piece
#                                 (if (null (equal (vacant) piece)) (progn
#                                         ;is this piece mine or theirs?
#                                         (setf minep (eq color piece-color))
#                                         ;evaluate now.
#                                         ;only add the value of the piece if it is not threatened.
#                                         (if (space-threatenedp ll_b color x y) (setf threatenedp T) (setf threatenedp NIL))
#                                         (if (or (equal piece "wp") (equal piece "bp")) (progn
#                                                 (if (and minep threatenedp) (setf value (- value (/ *pawn-value* 2))))
#                                                 (if minep
#                                                         (setf value (+ value *pawn-value*
#                                                                                                  (p-pos-val x y piece-color)
#                                                                                                  (* *pawn-move-value* (length (pawn-moves ll_b x y)))))
#                                                         (setf value (- value *pawn-value*
#                                                                                                  (p-pos-val x y piece-color)
#                                                                                                  (* *pawn-move-value* (length (pawn-moves ll_b x y)))))
#                                                 ))
#                                         (if (or (equal piece "wr") (equal piece "br")) (progn
#                                                 (if (and minep threatenedp) (setf value (- value (/ *rook-value* 2))))
#                                                 (if minep
#                                                         (setf value (+ value *rook-value*
#                                                                                                  (r-pos-val x y piece-color)
#                                                                                                  (* *rook-move-value* (length (rook-moves ll_b x y)))))
#                                                         (setf value (- value *rook-value*
#                                                                                                  (r-pos-val x y piece-color)
#                                                                                                  (* *rook-move-value* (length (rook-moves ll_b x y)))))
#                                                 ))
#                                         (if (or (equal piece "wn") (equal piece "bn")) (progn
#                                                 (if (and minep threatenedp) (setf value (- value (/ *knight-value* 2))))
#                                                 (if minep
#                                                         (setf value (+ value *knight-value*
#                                                                                                  (n-pos-val x y piece-color)
#                                                                                                  (* *knight-move-value* (length (knight-moves ll_b x y)))))
#                                                         (setf value (- value *knight-value*
#                                                                                                  (n-pos-val x y piece-color)
#                                                                                                  (* *knight-move-value* (length (knight-moves ll_b x y)))))
#                                                 ))
#                                         (if (or (equal piece "wb") (equal piece "bb")) (progn
#                                                 (if (and minep threatenedp) (setf value (- value (/ *bishop-value* 2))))
#                                                 (if minep
#                                                         (setf value (+ value *bishop-value*
#                                                                                                  (b-pos-val x y piece-color)
#                                                                                                  (* *bishop-move-value* (length (bishop-moves ll_b x y)))))
#                                                         (setf value (- value *bishop-value*
#                                                                                                  (b-pos-val x y piece-color)
#                                                                                                  (* *bishop-move-value* (length (bishop-moves ll_b x y)))))
#                                                 ))
#                                         (if (or (equal piece "wq") (equal piece "bq")) (progn
#                                                 ;special case for the queen... if your queen is threatened, and its not your turn,
#                                                 ; then just consider it lost.
#                                                         (if minep
#                                                                 (if (null threatenedp)
#                                                                         (setf value (+ value *queen-value*
#                                                                                                                  (q-pos-val x y piece-color)
#                                                                                                                  (* *queen-move-value* (length (queen-moves ll_b x y)))))
#                                                                 )
#                                                                 (setf value (- value *queen-value*
#                                                                                                          (q-pos-val x y piece-color)
#                                                                                                          (* *queen-move-value* (length (queen-moves ll_b x y)))))
#                                                         )
#                                                 )
#                                         (if (or (equal piece "wk") (equal piece "bk")) (progn
#                                                 (if (and minep threatenedp) (setf value (- value (/ *king-value* 2))))
#                                                 (if minep
#                                                         (setf value (+ value *king-value*
#                                                                                                  (k-pos-val x y piece-color)
#                                                                                                  (* *king-move-value* (length (king-moves ll_b x y)))))
#                                                         (setf value (- value *king-value*
#                                                                                                  (k-pos-val x y piece-color)
#                                                                                                  (* *king-move-value* (length (king-moves ll_b x y)))))
#                                                 ))
#                                         ))))))
#                                 ))
#                         )
#                 )
#                 (if (eq color *black*)
#                         (setf value (* -1 value))
#                 )
#
#                 (return-from evaluate-game-state value)
#         )
# )
