
adj_tile([X0,Y0], [X0,Y1]) :- =(Y1, Y0+1).
adj_tile([X0,Y0], [X0,Y1]) :- =(Y1, Y0-1).
adj_tile([X0,Y0], [X1,Y0]) :- =(X1, X0+1).
adj_tile([X0,Y0], [X1,Y0]) :- =(X1, X0-1).