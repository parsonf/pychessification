# ;;;; ###########################################################################
# ;;;;
# ;;;; This file contains functions relevant to printing information.
# ;;;;
# ;;;; ###########################################################################


def print_help():
    print('''\
Commands:
moves       - Displays your available moves.
help        - Displays this help message.
reset       - Starts a new game of chess.
end         - Ends the chess program.

How to Move:
Type the row and column of the piece to be moved,
followed by the row and column of the space to be moved to.
Here are some examples of moves:
e2e4        (pawn to e4)
g1f3        (knight to f3)
For castle moves, type the space of the king followed by
the space where the king will end up. For example:
e1g1        (white castle king-side)
e1c1        (white castle queen-side)

Other than that, enjoy the game!
''')


def print_board(board):
    print('''
    ##################################################''')
    for y in range(8):
        print('    ')
        if y % 2 == 0:
            print('#      +----+      +----+      +----+      +----+#')
        else:
            print('#+----+      +----+      +----+      +----+      #')
        print('', y-8, ' #', end='')
        for x in range(8):
            if x % 2 == 0:
                print('|', board[x][y], '|', end='')
            else:
                print(' ', board[x][y], '|', end='')
        print('#')
        print('    ', end='')
        if y % 2 == 0:
            print('#      +----+      +----+      +----+      +----+#')
        else:
            print('#+----+      +----+      +----+      +----+      #')
    print('''
        a     b     c     d     e     f     g     h
''')


def print_moves(moves):
    for move in moves:
        print_move(move, end='\n')


def print_move(move, end=''):
    is_row = True
    rows = ['a', 'b', 'c', 'd', 'e', 'f', 'g']

    for position in move:
        if is_row:
            print(rows[position], end='')
        else:
            print(position, end=end)
        is_row = not is_row
