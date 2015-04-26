:- module(mazeBig, [maze_size/2, barrier/2]).


maze_size(21, 21).

barrier(1	,4).
barrier(1	,4).
barrier(1	,8).
barrier(1	,6).
barrier(1	,2).
barrier(1	,6).
barrier(1	,12).
barrier(1	,1).
barrier(2	,20).
barrier(2	,16).
barrier(2	,4).
barrier(2	,2).
barrier(2   ,9).
barrier(2	,12).
barrier(2	,20).
barrier(3	,7).
barrier(3	,14).
barrier(3	,15).
barrier(3	,13).
barrier(3	,6).
barrier(3	,10).
barrier(3	,13).
barrier(3	,16).
barrier(3	,18).
barrier(3	,19).
barrier(3	,12).
barrier(4	,7).
barrier(4	,8).
barrier(4	,3).
barrier(4	,17).
barrier(4	,6).
barrier(4	,21).
barrier(4	,17).
barrier(5	,12).
barrier(5	,2).
barrier(5	,10).
barrier(5	,20).
barrier(5	,8).
barrier(5	,5).
barrier(5	,19).
barrier(5	,9).
barrier(5	,17).
barrier(5	,11).
barrier(6	,12).
barrier(6	,16).
barrier(6	,21).
barrier(6	,20).
barrier(6	,4).
barrier(6	,19).
barrier(6	,7).
barrier(6	,6).
barrier(6	,8).
barrier(6	,20).
barrier(7	,2).
barrier(7	,13).
barrier(7	,10).
barrier(7	,20).
barrier(7	,14).
barrier(7	,6).
barrier(7	,2).
barrier(7	,11).
barrier(7	,3).
barrier(7	,7).
barrier(8	,20).
barrier(8	,10).
barrier(8	,15).
barrier(8	,6).
barrier(8	,15).
barrier(8	,11).
barrier(8	,8).
barrier(8	,11).
barrier(8	,16).
barrier(8	,10).
barrier(9	,18).
barrier(9	,20).
barrier(9	,21).
barrier(9	,11).
barrier(9	,13).
barrier(9	,7).
barrier(9,	19).
barrier(10,	14).
barrier(10,	7).
barrier(10,	2).
barrier(10,	7).
barrier(10,	16).
barrier(10,	4).
barrier(10,	3).
barrier(10,	3).
barrier(11,	5).
barrier(11,	2).
barrier(11,	3).
barrier(11,	15).
barrier(11,	11).
barrier(11,	14).
barrier(11,	18).
barrier(11,	13).
barrier(11,	19).
barrier(11,	11).
barrier(12,	14).
barrier(12,	9).
barrier(12,	16).
barrier(12,	17).
barrier(12,	15).
barrier(12,	9).
barrier(12,	13).
barrier(12,	15).
barrier(12,	8).
barrier(12,	1).
barrier(13,	2).
barrier(13,	13).
barrier(13,	1).
barrier(13,	20).
barrier(13,	17).
barrier(13,	18).
barrier(13,	13).
barrier(13,	14).
barrier(13,	1).
barrier(13,	20).
barrier(14,	11).
barrier(14,	19).
barrier(14,	18).
barrier(14,	10).
barrier(14,	9).
barrier(14,	19).
barrier(14,	17).
barrier(14,	1).
barrier(14,	19).
barrier(14,	15).
barrier(15,	9).
barrier(15,	9).
barrier(15,	17).
barrier(15,	17).
barrier(15,	9).
barrier(15,	12).
barrier(15,	14).
barrier(15,	16).
barrier(15,	21).
barrier(15,	10).
barrier(16,	16).
barrier(16,	2).
barrier(16,	3).
barrier(16,	9).
barrier(16,	15).
barrier(16,	5).
barrier(16,	16).
barrier(16,	20).
barrier(16,	16).
barrier(16,	19).
barrier(17,	4).
barrier(17,	19).
barrier(17,	12).
barrier(17,	7).
barrier(17,	21).
barrier(17,	8).
barrier(17,	8).
barrier(17,	20).
barrier(17,	12).
barrier(17,	18).
barrier(18,	19).
barrier(18,	19).
barrier(18,	15).
barrier(18,	9).
barrier(18,	20).
barrier(18,	19).
barrier(18,	11).
barrier(18,	1).
barrier(18,	21).
barrier(18,	20).
barrier(19,	14).
barrier(19,	16).
barrier(19,	4).
barrier(19,	9).
barrier(19,	1).
barrier(19,	15).
barrier(19,	12).
barrier(19,	9).
barrier(19,	9).
barrier(19,	20).
barrier(20,	16).
barrier(20,	12).
barrier(20,	14).
barrier(20,	21).
barrier(20,	2).
barrier(20,	1).
barrier(20,	4).
barrier(20,	4).
barrier(20,	5).
barrier(20,	6).
barrier(21,	19).
barrier(21,	11).
barrier(21,	2).
barrier(21,	19).
barrier(21,	18).
barrier(21,	18).
barrier(21,	12).
barrier(21,	1).
barrier(21,	4).
barrier(21,	10).
barrier(1, 11).
barrier(1, 15).
barrier(1, 18).
barrier(4, 2).
barrier(4, 5).
barrier(4, 16).
barrier(5, 14).
barrier(8, 4).
barrier(8, 18).
barrier(9, 2).
barrier(10, 5).
barrier(10, 9).
barrier(11, 20).
barrier(12, 5).
barrier(12, 6).
barrier(12, 11).
barrier(13, 3).
barrier(13, 5).
barrier(13, 11).
barrier(14, 3).
barrier(14, 5).
barrier(14, 7).
barrier(15, 7).
barrier(16, 7).
barrier(16, 10).
barrier(17, 3).
barrier(17, 13).
barrier(18, 6).
barrier(18, 7).
barrier(18, 16).
barrier(19, 2).
barrier(19, 17).
barrier(19, 19).
barrier(20, 7).
barrier(20, 11).
barrier(21, 9).

 %%     1 2 3 4 5 6 7 8 9 101112131415161718192021
 %%   +-------------------------------------------+
 %% 1 | x x . x . x . x . . x x . . x . . x . . . |
 %% 2 | . x . x . . . . x . . x . . . x . . . x . |
 %% 3 | . . . . . x x . . x . . x x x x . x x . . |
 %% 4 | . x x . x x x x . . x . . . . x x . . . x |
 %% 5 | . x . . x . . x x x x x . x . . x . x x . |
 %% 6 | . . . x . x x x . . . x . . . x . . x x x |
 %% 7 | . x x . . x x . . x x . x x . . . . . x . |
 %% 8 | . . . x . x . x . x x . . . x x . x . x . |
 %% 9 | . x . . . . x . . . x . x . . . . x x x x |
 %% 10| . x x x x . x . x . . . . x . x . . . . . |
 %% 11| . x x . x . . . . . x . x x x . . x x x . |
 %% 12| x . . . x x . x x . x . x x x x x . . . . |
 %% 13| x x x . x . . . . . x . x x . . x x . x . |
 %% 14| x . x . x . x . x x x . . . x . x x x . . |
 %% 15| . . . . . . x . x x . x . x . x x . . . x |
 %% 16| . x x . x . x . x x . . . . x x . . x x . |
 %% 17| . . x x . . x x . . . x x . . . . x x x x |
 %% 18| x . . . . x x . x . x . . . x x . . x x x |
 %% 19| x x . x . . . . x . . x . x x x x . x x . |
 %% 20| x x . x x x x . . . x x . x . x . . . . x |
 %% 21| x x . x . . . . x x x x . . . . . x x . . |
 %%   +-------------------------------------------+