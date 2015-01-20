
adj_tile([X0,Y0], [X0,Y1]) :- (Y1 is Y0+1).
adj_tile([X0,Y0], [X0,Y1]) :- (Y1 is Y0-1).
adj_tile([X0,Y0], [X1,Y0]) :- (X1 is X0+1).
adj_tile([X0,Y0], [X1,Y0]) :- (X1 is X0-1).