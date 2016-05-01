# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains global variables for the chess program.
# ;;;;
# ;;;; ###########################################################################

# ;;;;;tweaking minimax algorithm
# ;------------------------------------
#         ;how many plies to search
PLY = 4

#         ;range within supposed-best-move's score to decide to evaluate move or prune the branch.
PRUNE_CUTOFF = 120

# ;;;;;tracking for castling
# ;------------------------------------
wk_has_moved = False
qs_wr_has_moved = False
ks_wr_has_moved = False

bk_has_moved = False
qs_br_has_moved = False
ks_br_has_moved = False

# ;;;;;helping readability
# ;------------------------------------
#         ; rows
A = 0
B = 1
C = 2
D = 3
E = 4
F = 5
G = 6
H = 7

#         ; colors of the pieces
WHITE = True
BLACK = False

#         ; checking for check
CHECK_CHECK = True
IGNORE_CHECK = False

#         ; check castling
CHECK_CASTLE = True
IGNORE_CASTLE = False

#         ; is a move an actual move, or just a hypothetical move. matters for castling rules..
ACTUAL_MOVE = True
HYPOTHETICAL_MOVE = True
