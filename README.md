# prolog

Open SWI-Prolog
run:
  change_working_directory(_, PATH_TO_FOLDER).
  ['load.pl'].

This will allow you to run solve/3 and printGrid/0
printGrid() - prints the grid the solver is working on.
solve(A, B, Path) - prints shortest Path from A to B, use ; for more paths.

To change the maze, open the load.pl file and uncomment/comment the relavent maze.
maze.pl is 5x9
maze-big.pl is 21x21
