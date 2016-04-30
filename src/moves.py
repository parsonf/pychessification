# (in-package :com.parsons.chessification)
# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains the rules of what moves are legal.
# ;;;;
# ;;;; Author: Brian S Parsons
# ;;;; Last modified: July 2014
# ;;;;
# ;;;; ###########################################################################
#
#
# ; this function gets all the legal moves for a given board and a given color.
# ; also, we will want to know if we should check for check and castle.
# ; why wouldn't we want to? checking for those involves checking for future moves,
# ; which would lead to an infinite loop.
# (defun get-all-legal-moves (ll_b color check-checkp check-castlep)
#         (let (
#                 (moves ())
#         )
#                 (loop :for y :from 1 :to 8 :do
#                         (loop :for x :from a :to h :do
#                                 ; we are iterating through each piece
#                                 (if (equal (vacant) (cb-at ll_b x y))
#                                         ; this is just a space. next.
#                                         NIL
#                                         ; when we find a piece...
#                                         (if (eq color (get-piece-color (cb-at ll_b x y)))
#                                                 ; if the piece found is the color we want...
#                                                 (setf moves (append moves (get-legal-moves ll_b x y)))
#                                         )
#                                 )
#                         )
#                 )
#
#                 ;check castling
#                 (if check-castlep
#                         (setf moves (append moves (castle-moves ll_b color)))
#                 )
#                 ; to prevent infinite loops, checking to see if a move puts/keeps you in check
#                 ; involves looking at every move the opponent can make and seeing if
#                 ; my king is captured with any of those moves. so, when that check
#                 ; is worked, we will not ask the question of whether or not the opponent
#                 ; has been put in check.  This makes sense, because it is irrelevant if I
#                 ; check the opponent while putting myself in check. My king will
#                 ; still be captured, thus the move wouldn't be legal anyway.
#                 ; Additionally, it is part of the rules (phew!):
#                 ; "3.9 The king is said to be 'in check' if it is attacked by one or
#                 ;  more of the opponent's pieces, even if such pieces are constrained
#                 ;  from moving to that square because they would then leave or place
#                 ;  their own king in check. No piece can be moved that will either
#                 ;  expose the king of the same colour to check or leave that king in check."
#                 ; This makes sense, because in chess, the goal is not to actually capture
#                 ; the king, but rather the game ends when the king cannot be removed
#                 ; from a threat. And a piece can still threaten the king even if it can't
#                 ; move due to it exposing its king, therefore, it is *necessary* to *not* check
#                 ; for check when looking at the available moves of the opponent after my move.
#                 ; Lastly, in the most corner case that comes to mind, it is not possible
#                 ; for the opponent to *currently* be in check if it is my turn,
#                 ; otherwise ***the game would be over***.
#                 (if check-checkp (progn
#                         (setf moves (check-check ll_b color moves))
#                 ))
#
#                 (return-from get-all-legal-moves moves)
#         )
# )
#
# ; see if a move puts you in check. if so, filter out the move.
# (defun check-check (ll_b color moves)
#         (let (
#                 (checked-moves ())
#                 (checkp-ll_b ())
#         )
#                 ; don't allow moves that result in being in check.
#                 ; loop through all potential moves.
#                 (loop for move in moves do
#                         ;reset the board to the current state
#                         (setf checkp-ll_b (copy-board ll_b))
#                         ;hypothetically make the move
#                         (board-move checkp-ll_b (first move) (second move) (third move) (fourth move) *hypothetical-move*)
#                         ;iff I am not in check, then the move is legal.
#                         (if (null (in-checkp checkp-ll_b color))
#                                 (setf checked-moves (append checked-moves (list move)))
#                         )
#                 )
#                 (return-from check-check checked-moves)
#         )
# )
#
# ; returns available castle moves for a given board and color.
# (defun castle-moves (ll_b color)
#         (let (
#                 (moves ())
#         )
#                 (if (eq color *white*) (progn
#                         ;white king can't have moved.
#                         (if (null *wk-has-moved*) (progn
#                                 ;king-side castle?
#                                                  ;king-side rook couldn't have moved.
#                                                  ;space between them are vacant.
#                                                  ;can't currently be in check nor spaces threatened.
#                                 (if (and (null *ks-wr-has-moved*)
#                                                  (equal (vacant) (cb-at ll_b f 1))
#                                                  (equal (vacant) (cb-at ll_b g 1))
#                                                  (null (space-threatenedp ll_b color e 1))
#                                                  (null (space-threatenedp ll_b color f 1))
#                                                  (null (space-threatenedp ll_b color g 1)))
#                                         (setf moves (append moves (list (list e 1 g 1))))
#                                 )
#                                 ;queen-side castle?
#                                                  ;queen-side rook couldn't have moved.
#                                                  ;space between them are vacant.
#                                                  ;can't currently be in check nor spaces threatened.
#                                 (if (and (null *qs-wr-has-moved*)
#                                                  (equal (vacant) (cb-at ll_b b 1))
#                                                  (equal (vacant) (cb-at ll_b c 1))
#                                                  (equal (vacant) (cb-at ll_b d 1))
#                                                  (null (space-threatenedp ll_b color e 1))
#                                                  (null (space-threatenedp ll_b color d 1))
#                                                  (null (space-threatenedp ll_b color c 1)))
#                                         (setf moves (append moves (list (list e 1 c 1))))
#                                 )
#                         ))
#                 ) (progn ; else, color is black
#                         ;black king can't have moved.
#                         (if (null *bk-has-moved*) (progn
#                                 ;king-side castle?
#                                                  ;king-side rook couldn't have moved.
#                                                  ;space between them are vacant.
#                                                  ;can't currently be in check nor spaces threatened.
#                                 (if (and (null *ks-br-has-moved*)
#                                                  (equal (vacant) (cb-at ll_b f 8))
#                                                  (equal (vacant) (cb-at ll_b g 8))
#                                                  (null (space-threatenedp ll_b color e 8))
#                                                  (null (space-threatenedp ll_b color f 8))
#                                                  (null (space-threatenedp ll_b color g 8)))
#                                         (setf moves (append moves (list (list e 8 g 8))))
#                                 )
#                                 ;queen-side castle?
#                                                  ;queen-side rook couldn't have moved.
#                                                  ;space between them are vacant.
#                                                  ;can't currently be in check nor spaces threatened.
#                                 (if (and (null *qs-br-has-moved*)
#                                                  (equal (vacant) (cb-at ll_b b 8))
#                                                  (equal (vacant) (cb-at ll_b c 8))
#                                                  (equal (vacant) (cb-at ll_b d 8))
#                                                  (null (space-threatenedp ll_b color e 8))
#                                                  (null (space-threatenedp ll_b color d 8))
#                                                  (null (space-threatenedp ll_b color c 8)))
#                                         (setf moves (append moves (list (list e 8 c 8))))
#                                 )
#                         ))
#                 ))
#                 (return-from castle-moves moves)
#         )
# )
#
# ; checks if a space is threatened by the opponent.
# (defun space-threatenedp (ll_b color x y)
#         (let (
#                 (threatenedp NIL)
#                 (moves ())
#         )
#                 (setf moves (get-all-legal-moves ll_b (null color) *ignore-check* *ignore-castle*))
#                 (loop for move in moves do
#                         (if (and (equal (third move) x) (equal (fourth move) y))
#                                 (setf threatenedp T)
#                         )
#                 )
#                 (return-from space-threatenedp threatenedp)
#         )
# )
#
# ; checks if a color is in check on a given board.
# (defun in-checkp (ll_b color)
#         (let (
#                 (in-check NIL)
#                 (moves ())
#         )
#                 ; if the destination of any move is a king of any color
#                 ; we ignore castle because castling cannot possibly capture opponent king,
#                 ; which is fortunate, because checking for it would likely result in
#                 ; another infinite loop.
#                 (setf moves (get-all-legal-moves ll_b (null color) *ignore-check* *ignore-castle*))
#                 ; loop through all the moves.
#                 ; if any destination threatens a space with a king on it, you're in check.
#                 (loop for move in moves do
#                         (if (member (cb-at ll_b (third move) (fourth move)) '("bk" "wk") :test #'equal)
#                                 (setf in-check T)
#                         )
#                 )
#                 (return-from in-checkp in-check)
#         )
# )
#
# ; gets all the legal moves for the piece at the given position on the given board.
# (defun get-legal-moves (ll_b x y)
#         (let (
#                 (moves NIL)
#                 (piece (cb-at ll_b x y))
#         )
#                 (if (or (equal piece "bp") (equal piece "wp")) (setf moves (pawn-moves ll_b x y))
#                 (if (or (equal piece "br") (equal piece "wr")) (setf moves (rook-moves ll_b x y))
#                 (if (or (equal piece "bn") (equal piece "wn")) (setf moves (knight-moves ll_b x y))
#                 (if (or (equal piece "bb") (equal piece "wb")) (setf moves (bishop-moves ll_b x y))
#                 (if (or (equal piece "bq") (equal piece "wq")) (setf moves (queen-moves ll_b x y))
#                 (if (or (equal piece "bk") (equal piece "wk")) (setf moves (king-moves ll_b x y))))))))
#
#                 (return-from get-legal-moves moves)
#         )
# )
#
# ; gets all the rook moves available at the given space on the given board.
# (defun rook-moves (ll_b x y)
#         (append ()
#                 (line-movement ll_b x y -1 0)           ;move left
#                 (line-movement ll_b x y 0 1)            ;move up
#                 (line-movement ll_b x y 1 0)            ;move right
#                 (line-movement ll_b x y 0 -1)           ;move down
#         )
# )
#
# ; gets all the bishop moves available at the given space on the given board.
# (defun bishop-moves (ll_b x y)
#         (append ()
#                 (line-movement ll_b x y -1 1)           ;move up left
#                 (line-movement ll_b x y 1 1)            ;move up right
#                 (line-movement ll_b x y 1 -1)           ;move down right
#                 (line-movement ll_b x y -1 -1)          ;move down left
#         )
# )
#
# ; gets all the queen moves available at the given space on the given board.
# (defun queen-moves (ll_b x y)
#         (append ()
#                 (line-movement ll_b x y -1 0)           ;move left
#                 (line-movement ll_b x y 0 1)            ;move up
#                 (line-movement ll_b x y 1 0)            ;move right
#                 (line-movement ll_b x y 0 -1)           ;move down
#                 (line-movement ll_b x y -1 1)           ;move up left
#                 (line-movement ll_b x y 1 1)            ;move up right
#                 (line-movement ll_b x y 1 -1)           ;move down right
#                 (line-movement ll_b x y -1 -1)          ;move down left
#         )
# )
#
# ; calculates movement from a given position
# ; on a given board in a given direction.
# ; returns all the available moves.
# (defun line-movement (ll_b x y x-shift y-shift)
#         (let (
#                 (moves ())
#                 (piece ())
#                 (color (get-piece-color (cb-at ll_b x y)))
#                 (not-blocked T)
#                 (new-x x)
#                 (new-y y)
#         )
#                 (loop do
#                         (setf new-x (+ new-x x-shift) new-y (+ new-y y-shift))
#                         ;if a valid space
#                         (if (valid-spacep new-x new-y) (progn
#                                         (setf piece (cb-at ll_b new-x new-y))
#                                         ;if vacant
#                                         (if (equal (vacant) piece)
#                                                 (setf moves (append moves (list (list x y new-x new-y)))) ; **** move left
#                                         ;else: not vacant.
#                                                 (progn
#                                                         ;blocked from further moves
#                                                         (setf not-blocked NIL)
#                                                         ;are the colors the same?
#                                                         (if (null (eq color     (get-piece-color piece)))
#                                                                 ;piece is an enemy
#                                                                 (setf moves (append moves (list (list x y new-x new-y)))) ; **** capture left
#                                                                 ;piece is friendly
#                                                         )
#                                                 )
#                                         )
#                         ))
#                 while (and (valid-spacep new-x new-y) not-blocked))
#                 (return-from line-movement moves)
#         )
# )
#
# ; gets all the knight moves available at the given space on the given board.
# (defun knight-moves (ll_b x y)
#         (let (
#                 (moves ())
#                 (color (get-piece-color (cb-at ll_b x y)))
#                 (new-x x)
#                 (new-y y)
#         )
#                 (setf new-x (+ x 1) new-y (+ y 2))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))                                               ; **** move up 2 right 1
#                 )
#                 (setf new-x (+ x 2) new-y (+ y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))                                               ; **** move up 1 right 2
#                 )
#                 (setf new-x (+ x 2) new-y (- y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))                                               ; **** move down 1 right 2
#                 )
#                 (setf new-x (+ x 1) new-y (- y 2))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))                                               ; **** move down 2 right 1
#                 )
#                 (setf new-x (- x 1) new-y (- y 2))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))                                               ; **** move down 2 left 1
#                 )
#                 (setf new-x (- x 2) new-y (- y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))                                               ; **** move down 1 left 2
#                 )
#                 (setf new-x (- x 2) new-y (+ y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))                                               ; **** move up 1 left 2
#                 )
#                 (setf new-x (- x 1) new-y (+ y 2))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))                                               ; **** move up 2 left 1
#                 )
#
#                 (return-from knight-moves moves)
#         )
# )
#
# ; gets all the king moves available at the given space on the given board.
# (defun king-moves (ll_b x y)
#         (let (
#                 (moves ())
#                 (color (get-piece-color (cb-at ll_b x y)))
#                 (new-x NIL)
#                 (new-y NIL)
#         )
#                 (setf new-x (- x 1) new-y y)
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))
#                 )
#                 (setf new-x (- x 1) new-y (+ y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))
#                 )
#                 (setf new-x x new-y (+ y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))
#                 )
#                 (setf new-x (+ x 1) new-y (+ y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))
#                 )
#                 (setf new-x (+ x 1) new-y y)
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))
#                 )
#                 (setf new-x (+ x 1) new-y (- y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))
#                 )
#                 (setf new-x x new-y (- y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))
#                 )
#                 (setf new-x (- x 1) new-y (- y 1))
#                 (if (and (valid-spacep new-x new-y) (null (friendly-occupiedp ll_b new-x new-y color)))
#                         (setf moves (append moves (list (list x y new-x new-y))))
#                 )
#
#                 (return-from king-moves moves)
#         )
# )
#
#
# ; gets all the pawn moves available at the given space on the given board.
# (defun pawn-moves (ll_b x y)
#         (let (
#                 (moves ())
#                 (color (get-piece-color (cb-at ll_b x y)))
#         )
#                 ;if color is white and y+1 is valid
#                 (if (and (eq *white* color) (valid-spacep x (+ y 1)))
#                         (progn
#                                 ;if y+1 is vacant, add move, check + 2
#                                 (if (equal (vacant) (cb-at ll_b x (+ y 1)))
#                                         (progn
#                                                 (setf moves (append moves (list (list x y x (+ y 1))))) ; **** up 1
#                                                 ;if y+2 is valid and vacant and row is 2, add move
#                                                 (if (and (valid-spacep x (+ y 2))
#                                                                  (equal (vacant) (cb-at ll_b x (+ y 2)))
#                                                                  (eq y 2))
#                                                         (setf moves (append moves (list (list x y x (+ y 2))))) ; **** up 2
#                                                 )
#                                         )
#                                 )
#                                 ;if x+1 is valid and x+1 y+1 is enemy occupied, add move
#                                 (if (and (valid-spacep (+ x 1) y)
#                                                  (enemy-occupiedp ll_b (+ x 1) (+ y 1) color))
#                                         (setf moves (append moves (list (list x y (+ x 1) (+ y 1))))) ; **** capture NE
#                                 )
#                                 ;if x-1 is valid and x-1 y+1 is enemy occupied, add move
#                                 (if (and (valid-spacep (- x 1) y)
#                                                  (enemy-occupiedp ll_b (- x 1) (+ y 1) color))
#                                         (setf moves (append moves (list (list x y (- x 1) (+ y 1))))) ; **** capture NW
#                                 )
#                         )
#                 )
#                 ;if color is black and y-1 is valid
#                 (if (and (eq *black* color) (valid-spacep x (- y 1)))
#                         (progn
#                                 ;if y-1 is vacant, add move, check - 2
#                                 (if (equal (vacant) (cb-at ll_b x (- y 1)))
#                                         (progn
#                                                 (setf moves (append moves (list (list x y x (- y 1))))) ; **** down 1
#                                                 ;if y-2 is valid and vacant and row is 7, add move
#                                                 (if (and (valid-spacep x (- y 2))
#                                                                  (equal (vacant) (cb-at ll_b x (- y 2)))
#                                                                  (eq y 7))
#                                                         (setf moves (append moves (list (list x y x (- y 2))))) ; **** down 2
#                                                 )
#                                         )
#                                 )
#                                 ;if x+1 is valid and x+1 y-1 is enemy occupied, add move
#                                 (if (and (valid-spacep (+ x 1) y)
#                                                  (enemy-occupiedp ll_b (+ x 1) (- y 1) color))
#                                         (setf moves (append moves (list (list x y (+ x 1) (- y 1))))) ; **** capture SE
#                                 )
#                                 ;if x-1 is valid and x-1 y-1 is enemy occupied, add move
#                                 (if (and (valid-spacep (- x 1) y)
#                                                  (enemy-occupiedp ll_b (- x 1) (- y 1) color))
#                                         (setf moves (append moves (list (list x y (- x 1) (- y 1))))) ; **** capture SW
#                                 )
#                         )
#                 )
#                 (return-from pawn-moves moves)
#         )
# )
#
