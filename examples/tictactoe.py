print("+------------------------+")
print("| Welcome to Tic Tac Toe |")
print("+------------------------+")
board = [["1", "2", "3"], ["4", "5", "6"], ["7", "8", "9"]]
def display_board():
  for row in board:
    row_string = ""
    for cell in row:
      row_string = ((row_string + cell) + " ")
    print(row_string)
def check_win():
  for row in board:
    if ((row[0] == row[1]) and (row[1] == row[2])):
      return row[0]
  for i in range(0, 2, 1):
    if ((board[0][i] == board[1][i]) and (board[1][i] == board[2][i])):
      return board[0][i]
  if ((board[0][0] == board[1][1]) and (board[1][1] == board[2][2])):
    return board[0][0]
  if ((board[0][2] == board[1][1]) and (board[1][1] == board[2][0])):
    return board[0][2]
  return False
def check_draw():
  for row in board:
    for cell in row:
      if ((cell != "X") and (cell != "O")):
        return False
  return True
current_player = "X"
while True:
  display_board()
  print((("Player " + current_player) + ", enter your move (1-9): "))
  move = int(input())
  if ((move < 1) or (move > 9)):
    print("Invalid move. Please try again.")
    continue
  row = ((move - 1) // 3)
  col = ((move - 1) % 3)
  if ((board[row][col] == "X") or (board[row][col] == "O")):
    print("Cell already occupied. Please try again.")
    continue
  board[row][col] = current_player
  if check_win():
    display_board()
    print((("Player " + current_player) + " wins!"))
    break
  if check_draw():
    display_board()
    print("Game is a draw!")
    break
  if (current_player == "X"):
    current_player = "O"
  else:
    current_player = "X"