# prolog

Open SWI-Prolog
run:
  change_working_directory(_, PATH_TO_FOLDER).
  ['load.pl'].

This will allow you to run mazeSolver.solve/3 and gridPrinter:printGrid/0
gridPrinter:printGrid() - prints the grid the solver is working on.
mazeSolver.solve(A, B, Path) - prints shortest Path from A to B, use ; for more paths.

To change the maze, open the load.pl file and uncomment/comment the relavent maze.
maze.pl is 5x9
maze-big.pl is 21x21
