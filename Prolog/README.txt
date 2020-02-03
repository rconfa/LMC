Main features:
The file that I use in the example is: reverse.lmc

-Reads an assembly file and produces the relative initial memory list.
	?- lmc_load("reverse.lmc", Mem).
		
	   Mem = [901, 712, 332, 536, 134, 327, 532, 627, 536, 231, 336, 600, 536, 131, 336, 817, 0, 536, 133, 329, 
	   629, 902, 536, 131, 336, 135, 615, 0, 608, 0, 621, 1, 0, 500, 300, 900, 99, 63, 52, 4, 
	   61, 15, 40, 27, 30, 53, 78, 95, 37, 76, 60, 25, 64, 82, 94, 33, 93, 41, 3, 72, 
	   36, 77, 57, 35, 49, 48, 87, 74, 16, 14, 47, 65, 86, 1, 2, 92, 13, 83, 50, 38, 
	   22, 11, 5, 45, 96, 34, 54, 89, 9, 18, 91, 29, 20, 55, 66, 67, 42, 19, 69, 31]
	   
- Executes the single instruction pointed by the program counter. 
	?- one_instruction(state(0, 0, [105, 0, 901, 371, 901, 372, ...], [], [], noflag), Out).

	Out = state(372, 1, [105, 0, 901, 371, 901, 372|...], [], [], noflag).

-Directly executes a given initial memory list and his input values and produces the output list.
	?- execution_loop(state(0, 0, [901, 370, 901, 371, 901, 372, 901, 373, 901, 374, 
								   578, 375, 571, 270, 823, 577, 375, 570, 376, 571, 
								   370, 576, 371, 572, 271, 834, 577, 375, 571, 376, 
								   572, 371, 576, 372, 573, 272, 845, 577, 375, 572, 
								   376, 573, 372, 576, 373, 574, 273, 856, 577, 375, 
								   573, 376, 574, 373, 576, 374, 575, 759, 610, 570, 
								   902, 571, 902, 572, 902, 573, 902, 574, 902, 0, 
								   0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
								   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
								   0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
								   [5, 2, 6, 7, 8], [], noflag), Out).

		Out = [2, 5, 6, 7, 8].
	
-Reads an assembly file, produces and executes the relative initial memory list with the given input list.
	?- lmc_run("reverse.lmc", [1, 2, 3, 4, 5, 6, 0, 1, 2], Out).

		Out = [6, 5, 4, 3, 2, 1]


Extra notes:
-I check that the memory list in the LMC state have elements correct if I use them, otherwise 
I assume that all elements are correct. Moreover I check that the length of the memory list 
is 100 and that the values in the input list are valid if I use them.

-I print an error message if something goes wrong and I fail the program. 

-I accept labels that start with alpha characters or underscore and don't contain special characters 
like exclamation point or number sign. I also don't accept numerical number for labels.



Thanks for reading!
