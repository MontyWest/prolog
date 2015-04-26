# prolog
## Maze solver in prolog

---------------------

### Author:
Monty West - mwest06

### Running:
- Clone repo and move to directory
- Run
	```
	$ swipl
	> ['load']
	```
- You can now use:
	```
	%%Prints the maze
	printMaze.

	%%Finds shortest path from [4,10] to [19,7] (which has multiple shortest paths, use ';' to see them all)
	%%Substitute these coordinates to find other paths. Coordinates are [Row, Column]
	solve([4,10],[19,7],Path). 
	```
	
### Changing maze:
- In **load.pl** you can change the maze by commenting/uncommenting the maze lines, it is currently set to **maze-big.pl**.
- You can also make your own maze file. Simply declare maze_size/2 and barrier/2 e.g.
 	```
 	:- module(myMaze, [maze_size/2, barrier/2]).
 	maze_size(3, 3).
	barrier(2, 2).
	barrier(2, 3).
	```