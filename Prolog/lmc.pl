%%%% -*- Mode: Prolog -*-
%%%% LMC.pl
%%%% A simulator for the Little Man Computer.

%%% 829914 Cazzaniga Elisa
%%% 830404 Confalonieri Riccardo




%%% lmc_run/3
%%% Loads the file (Filename) and creates the corresponding numerical
%%% list (Mem). 
%%% [Note1] Then executes every instruction starting from 0, with 
%%% the input list given (In) and produces the corresponding output (Out).

lmc_run(Filename, In, Out) :-
	lmc_load(Filename, Mem),
	execution_loop(state(0, 0, Mem, In, [], noflag), Out). % see [Note1] above.


%%% lmc_load/2
%%% Reads the file (Filename), adjusts all the strings readed and saves
%%% the result in a list (ListToBeParsed). After save all the
%%% labels (ListLabel)  and check that there aren't duplicate labels.
%%% The ListLabel contains all the defined label names and their
%%% position like [NameLabel1, 0, NameLabel2, 9] where 0 and 9 are the
%%% positions in which the labels are defined. Next converts all
%%% instructions into the corresponding numerical code (MemToFill) and
%%% checks that the list is fill correctly. If all these controls are
%%% true, the predicate generates the list (Mem).

lmc_load(Filename, Mem) :-
	file_to_string(Filename, FileString),
	adjust_before_parsing(FileString, ListToBeParsed),
	check_syntax_save_label(ListToBeParsed, ListLabel, 0),
	noduplicate(ListLabel),
	convert(ListToBeParsed, ListLabel, MemToFill),
	check_if_fill(MemToFill, Mem).


%%% file_to_string/2
%%% True if the file (Filename) exists and can be opened and read into a
%%% string (FileString). Else the predicate fails with an error
%%% message (also the whole program fails).

file_to_string(Filename, FileString) :-
	catch((open(Filename, read, Stream),
	       read_string(Stream, _, FileString),
	       close(Stream)),
	      _,
	      (print_bold("File not found."),
	      fail)).


%%% adjust_before_parsing/2
%%% Splits the string on newline and saves the result in a list
%%% (FileList), then corrects the list by removing all comments and
%%% converting to lower case each string of the list and saves it
%%% (FileListNoComments). Deletes all empty strings (ListToBeParsed) and
%%% checks that the total length of the list does not exceed 100. This
%%% predicate fails only when the list (ListToBeParsed) contains more
%%% than 100 instruction.

adjust_before_parsing(FileString, ListToBeParsed) :-
	split_string(FileString, "\n", "\n", FileList),
	correct_string(FileList, FileListNoComments),
	delete(FileListNoComments, "", ListToBeParsed),
	length(ListToBeParsed, Len),
	Len < 101,
	!.


%%% adjust_before_parsing/2
%%% If the list contains more than 100 instruction the predicate fails
%%% with an error message (also the whole program fails).

adjust_before_parsing(_, _) :-
	print_bold("File too large"),
	fail.


%%% correct_string/2
%%% Deletes all comments present in the string and converts each string
%%% into lower string. If the list of string is empty returns an empty
%%% list.

correct_string([], []).


%%% correct_string/2
%%% True if the string contains comments that need to be delete.

correct_string([StartString | ReadList], [FinalString | ListNoComments]) :-
	clean_comment(StartString, StringWithComment),
	!,
	string_lower(StringWithComment, StringNoComment),
	clean_extra_space(StringNoComment, FinalString),
	correct_string(ReadList, ListNoComments).


%%% correct_string/2
%%% The string doesn't contain comments.

correct_string([StartString | ReadList], [FinalString | ListNoComments]) :-
	!,
	string_lower(StartString, StringWithSpace),
	clean_extra_space(StringWithSpace, FinalString),
	correct_string(ReadList, ListNoComments).


%%% clean_comment/2
%%% If the string contains a comment it will be deleted by performing two
%%% substring. If the string don't contains a comment it will fail.

clean_comment(String, NewString) :-
	sub_string(String, StartIndex, _, _, "//"),
	sub_string(String, 0, StartIndex, _, NewString).


%%% check_if_fill/2
%%% Check if the length of the list is 100.
%%% If it's true it returns the same list (List).

check_if_fill(List, List) :-
	length(List, Len),
	Len = 100,
	!.


%%% check_if_fill/2
%%% The list (List) contains less than 100 elements so it need to be fill.

check_if_fill(List, NewList) :-
	length(List, Len),
	Len < 100,
	!,
	fill_mem(List, NewList).


%%% fill_mem/2
%%% If the list is empty it will be fill with
%%% a starting value 0 then will be recursively filled.

fill_mem([],NewList):-
	!,
	append([],[0],TempList),
	fill_mem(TempList,NewList).


%%% fill_mem/2
%%% If the list is not empty and is length is less than 100
%%% this predicate append a new 0 and continue to
%%% recursively fill the list.

fill_mem(List,NewList):-
	length(List,Len),
	Len < 100,
	!,
	append(List,[0],TempList),
	fill_mem(TempList,NewList).


%%% fill_mem/2
%%% The length of the list is 100, so it will be returned without
%%% doing nothing.

fill_mem(List,List):-
	!.


%%% clean_extra_space/2
%%% Normalizes all spaces in the string (SpaceString) and saves it
%%% into (TempNormalString) then reconverts TempNormalString into
%%% a string (NormalString).

clean_extra_space(SpaceString, NormalString) :-
	normalize_space(atom(TempFinalString), SpaceString),
	atom_string(TempFinalString, NormalString).


%%% noduplicate/1
%%% Checks if the list doesn't contain duplicate.
%%% True if the list is empty.

noduplicate([]).


%%% noduplicate/1
%%% True if the list contains two equal members (LabelName).
%%% The predicate fails with an error message (also the whole program fails)

noduplicate([LabelName | Ts]) :-
	member(LabelName, Ts),
	!,
	print_bold("Error, invalid label name: "),
	print_bold(LabelName),
	fail.


%%% noduplicate/1
%%% The first element of the list is not duplicate.

noduplicate([_ | T]) :-
	noduplicate(T).


%%% convert/3
%%% Converts every string instruction into numerical value by count the spaces
%%% between the words.
%%% If the list with the string instruction is empty it returns an empty list.

convert([], _, []).


%%% convert/3
%%% Counts the number of spaces in the string (Instruction) and with
%%% the predicate convert_helper stores the corresponding numerical value (Val)
%%% into the list Mem.

convert([Instruction | ListInstruction], ListLabel, [Val | Mem]) :-
	count_space(Instruction, SpaceNumber),
	convert_helper(Instruction, ListLabel, Val, SpaceNumber),
	convert(ListInstruction, ListLabel, Mem).


%%% convert_helper/4
%%% Converts the istruction based of the number of spaces (SpaceNumber) between
%%% the words. If the SpaceNumber is 0 the istruction must be an instruction
%%% without any further value like "dat/inp..". The numerical value (Val) is
%%% obtained giving to select_operation the entire string Instruction.
%%% [Note2] the cut is in this position because otherwise the predicate never
%%% will not unify with the error case.

convert_helper(Instruction, _, Val, SpaceNumber) :-
	SpaceNumber = 0,
	is_single_instruction(Instruction),
	select_operation(Instruction, _, _, Val),
	!. % see [Note2] above.


%%% convert_helper/4
%%% If the SpaceNumber is 1 the istruction could be an instruction with a value
%%% or label like "add xx", where xx could be a number or a label name.
%%% The numerical value (Val) is obtained giving to select_operation the name
%%% of the instruction (S1), his value (S2) and the ListLabel because
%%% S2 could be a label.
%%% [Note3] the cut is in this position because otherwise the predicate never
%%% will not unify with the error case.

convert_helper(Instruction, ListLabel, Val, SpaceNumber) :-
	SpaceNumber = 1,
	split_string(Instruction, " ", " ", [S1, S2 | _]),
	is_instruction_with_value(S1),
	select_operation(S1, S2, ListLabel, Val),
	!.  % see [Note3] above.


%%% convert_helper/4
%%% If the SpaceNumber is 1 the istruction could be the define of a label with
%%% a single istruction like "label dat".
%%% The first part of the string (LabelName) must be a member of the ListLabel
%%% otherwise the predicate fails. The numerical value (Val) is obtained giving
%%% to select_operation only the single instruction (StringInstruction),
%%% the label name is useless because is the definition of a label and not
%%% its use.
%%% [Note4] the cut is in this position because otherwise the predicate never
%%% will not unify with the error case.

convert_helper(Instruction, ListLabel, Val, SpaceNumber) :-
	SpaceNumber = 1,
	split_string(Instruction, " ", " ", [LabelName, StringInstruction | _]),
	member(LabelName, ListLabel),
	is_single_instruction(StringInstruction),
	select_operation(StringInstruction, _, _, Val),
	!. % see [Note4] above.


%%% convert_helper/4
%%% If the SpaceNumber is 2 the istruction could be the define of a label with
%%% an instruction with value like "label add xx", where xx could be a number
%%% or a label name.
%%% The first part of the string (LabelName) must be a member of the ListLabel
%%% otherwise the predicate fails. The second part (S2) must be an istruction
%%% with value. The numerical value (Val) is obtained giving
%%% to select_operation the name of the instruction (S2), his value (S3) and
%%% ListLabel because S3 could be a label.
%%% [Note5] the cut is in this position because otherwise the predicate never
%%% will not unify with the error case.

convert_helper(Instruction, ListLabel, Val, SpaceNumber) :-
	SpaceNumber = 2,
	split_string(Instruction, " ", " ", [LabelName, S2, S3 | _]),
	member(LabelName, ListLabel),
	is_instruction_with_value(S2),
	select_operation(S2, S3, ListLabel, Val),
	!. % see [Note5] above.


%%% convert_helper/4
%%% If all the previous cases fail the instruction can't be converted so
%%% the predicate fails with an error message (also the whole program fails).

convert_helper(Instruction, _, _, _) :-
	!,
	print_bold("Error, invalid instruction: "),
	print_bold(Instruction),
	fail.


%%% select_operation/4
%%% Takes a correct instruction name (InstName), his argument value that could
%%% be a number or a valid label name (Arg1), the list with the name of all
%%% labels and their position (ListLabel) and generates the corresponding value
%%% for the memory (ValMem).
%%% If the InstName is "add" the ValMem is calculated by op_add.

select_operation(InstName, Arg1, ListLabel, ValMem) :-
	InstName = "add",
	!,
	op_add(Arg1, ListLabel, ValMem).


%%% select_operation/4
%%% If the InstName is "sub" the ValMem is calculated by op_sub.

select_operation(InstName, Arg1, ListLabel, ValMem) :-
	InstName = "sub",
	!,
	op_sub(Arg1, ListLabel, ValMem).


%%% select_operation/4
%%% If the InstName is "sta" the ValMem is calculated by op_sta.

select_operation(InstName, Arg1, ListLabel, ValMem) :-
	InstName = "sta",
	!,
	op_sta(Arg1, ListLabel, ValMem).


%%% select_operation/4
%%% If the InstName is "lda" the ValMem is calculated by op_lda.

select_operation(InstName, Arg1, ListLabel, ValMem) :-
	InstName = "lda",
	!,
	op_lda(Arg1, ListLabel, ValMem).


%%% select_operation/4
%%% If the InstName is "bra" the ValMem is calculated by op_bra.

select_operation(InstName, Arg1, ListLabel, ValMem) :-
	InstName = "bra",
	!,
	op_bra(Arg1, ListLabel, ValMem).


%%% select_operation/4
%%% If the InstName is "brz" the ValMem is calculated by op_bzr.

select_operation(InstName, Arg1, ListLabel, ValMem) :-
	InstName = "brz",
	!,
	op_brz(Arg1, ListLabel, ValMem).


%%% select_operation/4
%%% If the InstName is "brp" the ValMem is calculated by op_brp.

select_operation(InstName, Arg1, ListLabel, ValMem) :-
	InstName = "brp",
	!,
	op_brp(Arg1, ListLabel, ValMem).


%%% select_operation/4
%%% If the InstName is "inp" the ValMem is 901.

select_operation(InstName, _, _, ValMem) :-
	InstName = "inp",
	!,
	ValMem = 901.


%%% select_operation/4
%%% If the InstName is "out" the ValMem is 902.

select_operation(InstName, _, _, ValMem) :-
	InstName = "out",
	!,
	ValMem = 902.


%%% select_operation/4
%%% If the InstName is "hlt" the ValMem is 0.

select_operation(InstName, _, _, ValMem) :-
	InstName = "hlt",
	!,
	ValMem = 0.


%%% select_operation/4
%%% If the InstName is "dat" and the argument of the instruction (Arg1) exists
%%% the ValMem is calculated by op_dat.

select_operation(InstName, Arg1, _, ValMem) :-
	InstName = "dat",
	string(Arg1),
	!,
	op_dat(Arg1, _, ValMem).


%%% select_operation/4
%%% If the InstName is "dat" and the argument of the instruction doesn't exist
%%% the ValMem is 0.

select_operation(InstName, _, _, ValMem) :-
	InstName = "dat",
	!,
	ValMem = 0.


%%% op_add/3
%%% If the StringArg is a number and his value is in the range [0,99],
%%% it will be concatenated with the specific number for the operation add,
%%% "1", and saved into ValMem.

op_add(StringArg, _, ValMem) :-
	number_string(NumberArg, StringArg),
	!,
	NumberArg >= 0,
	NumberArg < 100,
	my_number_concat("1", NumberArg, ValMem).


%%% op_add/3
%%% If the StringArg is not a number it must be a valid label name.
%%% I take the Index of the define of the label from the ListLabel, add 1 to
%%% this index to take the number corresponding (IndexCorrect) and not the name
%%% of the label. Then IndexCorrect is concatenated with the specific number
%%% for the operation add, "1" and saved into ValMem.

op_add(StringArg, ListLabel, ValMem) :-
	!,
	member(StringArg, ListLabel),
	nth0(Index, ListLabel, StringArg),
	IndexCorrect is Index + 1,
	nth0(IndexCorrect, ListLabel, NumberArg),
	my_number_concat("1", NumberArg, ValMem).


%%% op_sub/3
%%% If the StringArg is a number and his value is in the range [0,99],
%%% it will be concatenated with the specific number for the operation sub,
%%% "2", and saved into ValMem.

op_sub(StringArg, _, ValMem) :-
	number_string(NumberArg, StringArg),
	!,
	NumberArg >= 0,
	NumberArg < 100,
	my_number_concat("2", NumberArg, ValMem).


%%% op_sub/3
%%% If the StringArg is not a number it must be a valid label name.
%%% I take the Index of the define of the label from the ListLabel, add 1 to
%%% this index to take the number corresponding (IndexCorrect) and not the name
%%% of the label. Then IndexCorrect is concatenated with the specific number
%%% for the operation sub, "2" and saved into ValMem.

op_sub(StringArg, ListLabel, ValMem) :-
	!,
	member(StringArg, ListLabel),
	nth0(Index, ListLabel, StringArg),
	IndexCorrect is Index +1,
	nth0(IndexCorrect, ListLabel, NumberArg),
	my_number_concat("2", NumberArg, ValMem).


%%% op_sta/3
%%% If the StringArg is a number and his value is in the range [0,99],
%%% it will be concatenated with the specific number for the operation sta,
%%% "3", and saved into ValMem.

op_sta(StringArg, _, ValMem) :-
	number_string(NumberArg, StringArg), %%%se arg è un Int
	!,
	NumberArg >= 0,
	NumberArg < 100,
	my_number_concat("3", NumberArg, ValMem).


%%% op_sta/3
%%% If the StringArg is not a number it must be a valid label name.
%%% I take the Index of the define of the label from the ListLabel, add 1 to
%%% this index to take the number corresponding (IndexCorrect) and not the name
%%% of the label. Then IndexCorrect is concatenated with the specific number
%%% for the operation sta, "3" and saved into ValMem.

op_sta(StringArg, ListLabel, ValMem) :-
	!,
	member(StringArg, ListLabel),
	nth0(Index, ListLabel, StringArg),
	IndexCorrect is Index + 1,
	nth0(IndexCorrect, ListLabel, NumberArg),
	my_number_concat("3", NumberArg, ValMem).


%%% op_lda/5
%%% If the StringArg is a number and his value is in the range [0,99],
%%% it will be concatenated with the specific number for the operation lda,
%%% "5", and saved into ValMem.

op_lda(StringArg, _, ValMem) :-
	number_string(NumberArg, StringArg),
	!,
	NumberArg >= 0,
	NumberArg < 100,
	my_number_concat("5", NumberArg, ValMem).


%%% op_lda/3
%%% If the StringArg is not a number it must be a valid label name.
%%% I take the Index of the define of the label from the ListLabel, add 1 to
%%% this index to take the number corresponding (IndexCorrect) and not the name
%%% of the label. Then IndexCorrect is concatenated with the specific number
%%% for the operation lda, "5" and saved into ValMem.

op_lda(StringArg, ListLabel, ValMem) :-
	!,
	member(StringArg, ListLabel),
	nth0(Index, ListLabel, StringArg),
	IndexCorrect is Index + 1,
	nth0(IndexCorrect, ListLabel, NumberArg),
	my_number_concat("5", NumberArg, ValMem).


%%% op_bra/3
%%% If the StringArg is a number and his value is in the range [0,99],
%%% it will be concatenated with the specific number for the operation bra,
%%% "6", and saved into ValMem.

op_bra(StringArg, _, ValMem) :-
	number_string(NumberArg, StringArg),
	!,
	NumberArg >= 0,
	NumberArg < 100,
	my_number_concat("6", NumberArg, ValMem).


%%% op_bra/3
%%% If the StringArg is not a number it must be a valid label name.
%%% I take the Index of the define of the label from the ListLabel, add 1 to
%%% this index to take the number corresponding (IndexCorrect) and not the name
%%% of the label. Then IndexCorrect is concatenated with the specific number
%%% for the operation bra, "6" and saved into ValMem.

op_bra(StringArg, ListLabel, ValMem) :-
	!,
	member(StringArg, ListLabel),
	nth0(Index, ListLabel, StringArg),
	IndexCorrect is Index + 1,
	nth0(IndexCorrect, ListLabel, NumberArg),
	my_number_concat("6", NumberArg, ValMem).


%%% op_brz/3
%%% If the StringArg is a number and his value is in the range [0,99],
%%% it will be concatenated with the specific number for the operation brz,
%%% "7", and saved into ValMem.

op_brz(StringArg, _, ValMem) :-
	number_string(NumberArg, StringArg),
	!,
	NumberArg >= 0,
	NumberArg < 100,
	my_number_concat("7", NumberArg, ValMem).


%%% op_brz/3
%%% If the StringArg is not a number it must be a valid label name.
%%% I take the Index of the define of the label from the ListLabel, add 1 to
%%% this index to take the number corresponding (IndexCorrect) and not the name
%%% of the label. Then IndexCorrect is concatenated with the specific number
%%% for the operation brz, "7" and saved into ValMem.

op_brz(StringArg, ListLabel, ValMem) :-
	!,
	member(StringArg, ListLabel),
	nth0(Index, ListLabel, StringArg),
	IndexCorrect is Index + 1,
	nth0(IndexCorrect, ListLabel, NumberArg),
	my_number_concat("7", NumberArg, ValMem).


%%% op_brp/3
%%% If the StringArg is a number and his value is in the range [0,99],
%%% it will be concatenated with the specific number for the operation brp,
%%% "8", and saved into ValMem.

op_brp(StringArg, _, ValMem) :-
	number_string(NumberArg, StringArg),
	!,
	NumberArg >= 0,
	NumberArg < 100,
	my_number_concat("8", NumberArg, ValMem).


%%% op_brp/3
%%% If the StringArg is not a number it must be a valid label name.
%%% I take the Index of the define of the label from the ListLabel, add 1 to
%%% this index to take the number corresponding (IndexCorrect) and not the name
%%% of the label. Then IndexCorrect is concatenated with the specific number
%%% for the operation brp, "8" and saved into ValMem.

op_brp(StringArg, ListLabel, ValMem) :-
	!,
	member(StringArg, ListLabel),
	nth0(Index, ListLabel, StringArg),
	IndexCorrect is Index + 1,
	nth0(IndexCorrect, ListLabel, NumberArg),
	my_number_concat("8", NumberArg, ValMem).


%%% op_dat/3
%%% If the StringArg is a number and his value is in the range [0,99],
%%% the ValMem is exactly the number.

op_dat(StringArg, _, ValMem) :-
	number_string(NumberArg, StringArg),
	!,
	NumberArg >= 0,
	NumberArg < 1000,
	ValMem = NumberArg.


%%% my_number_concat/3
%%% If the number (Int) that as to be concat is less than 10, I concatenate a 0
%%% between the FirstChar and Int, then I reconvert it from atom to number.
%%% like my_number_concat(1, 9, X) --> X = 109.

my_number_concat(FirstChar, Int, Value) :-
	Int < 10,
	!,
	atom_concat("0", Int, IntDoubleDigit),
	atom_concat(FirstChar, IntDoubleDigit, AtomValue),
	atom_number(AtomValue, Value).


%%% my_number_concat/3
%%% The number (Int) to concat is more than 10, I can directly concatenate the
%%% FirstChar with Int, then I reconvert it from atom to number.

my_number_concat(FirstChar, Int, Value) :-
	!,
	atom_concat(FirstChar, Int, AtomValue),
	atom_number(AtomValue, Value).


%%% check_syntax_save_label/3
%%% It starts from the given index to check if all the string in the
%%% OperationList respect the assembly sintax:
%%% Instruction (space = 0)
%%% Instruction xx (space = 1, xx could be a number or a label)
%%% label instruction (space = 1, Define of a label with a single instruction)
%%% label istruction xx (space = 2, Define of a label follow by
%%%					  instruction with value. XX could be a number or a label)
%%% Morover this predicate saves all the declarations of the labels and their
%%% position in ListLabel.
%%% This predicate fails if an instruction is wrong.
%%% If the OperationList is empty all the instructions are correct and ListLabel
%%% will be an empty list.

check_syntax_save_label([], ListLabel, _):-
	!,
	ListLabel = [].


%%% check_syntax_save_label/3
%%% Case: definition of a label.
%%% Counts the spaces of the first string in the list (FirstOp), checks if
%%% FirstOp is valid and saves the name of the label (Elem) and his Index
%%% in the ListLabel.
%%% It saves only if is a declaration, so Elem will be a name and not an empty
%%% string.

check_syntax_save_label([FirstOp | OperationList],
			[Elem, ListIndex | ListLabel],
			ListIndex) :-
	count_space(FirstOp, SpaceNumber),
	check_syntax_save_label_helper(FirstOp, Elem, SpaceNumber),
	Elem \= "",
	!,
	ListIndex2 is ListIndex + 1,
	check_syntax_save_label(OperationList, ListLabel, ListIndex2).


%%% check_syntax_save_label/3
%%% Case: all the other case, not the definition of a label.
%%% Counts the spaces of the first string in the list (FirstOp) and checks if
%%% is FirstOp is valid. In this case Elem must be an empty String.

check_syntax_save_label([FirstOp | OperationList], ListLabel, ListIndex) :-
	count_space(FirstOp, SpaceNumber),
	check_syntax_save_label_helper(FirstOp, Elem, SpaceNumber),
	Elem = "",
	!,
	ListIndex2 is ListIndex + 1,
	check_syntax_save_label(OperationList, ListLabel, ListIndex2).


%%% check_syntax_save_label_helper/3
%%% The first instruction is not a valid instruction.
%%% The predicate fails with an error message (also the whole program fails).

check_syntax_save_label([FirstOp | _], _, _) :-
	!,
	print_bold("Error, invalid instruction: "),
	print_bold(FirstOp),
	fail.


%%% check_syntax_save_label_helper/3
%%% Controls if the given operation (FirstOp) is valid.
%%% case: space = 2
%%% Is the definition of a new label. The first part (S1) must be a valid
%%% label name and the second one (S2) must be a valid
%%% instruction with value name.
%%% If all it's correct saves the label name (LabelName).

check_syntax_save_label_helper(FirstOp, LabelName, SpaceNumber) :-
	SpaceNumber = 2,
	split_string(FirstOp, " ", " ", [S1, S2 | _]),
	valid_label(S1),
	is_instruction_with_value(S2),
	!,
	LabelName = S1.


%%% check_syntax_save_label_helper/3
%%% case: space = 1
%%% It could be the definition of a new label or an instruction with value.
%%% In the first case the first part (S1) must be a valid label name and the
%%% second one must be a single instruction (dat/inp..)
%%% If all it's correct saves the label name (LabelName).

check_syntax_save_label_helper(FirstOp, LabelName, SpaceNumber) :-
	SpaceNumber = 1,
	split_string(FirstOp, " ", " ", [S1, S2 | _]),
	valid_label(S1),
	is_single_instruction(S2),
	!,
	LabelName = S1.


%%% check_syntax_save_label_helper/3
%%% case: space = 1
%%% Is an instruction with value. The first part of the FirstOp (S1) must be
%%% a valid istruction with value.
%%% The LabelName will be an empty string because it isn't the definition of
%%% a new label.

check_syntax_save_label_helper(FirstOp, LabelName, SpaceNumber) :-
	SpaceNumber = 1,
	split_string(FirstOp, " ", " ", [S1, _ | _]),
	is_instruction_with_value(S1),
	!,
	LabelName = "".


%%% check_syntax_save_label_helper/3
%%% case: space = 0
%%% checks if the FirstOp is a single instruction.
%%% The LabelName will be an empty string because it isn't the definition of
%%% a new label.

check_syntax_save_label_helper(FirstOp, LabelName, SpaceNumber) :-
	SpaceNumber = 0,
	is_single_instruction(FirstOp),
	!,
	LabelName = "".


%%% count_space/2
%%% Counts all the spaces present in the given String.

count_space(String, Cont) :-
	string_codes(String, Vect),
	findall(32, member(32, Vect), X),
	length(X, Cont).


%%% valid_label/1
%%% True if the String is a valid label.
%%% It starts converting the string into his number code, then I check if the
%%% first char (H) of the String could be accepted and then I check the rest
%%% of the string (Xs).

valid_label(String) :-
	not_instruction(String),
	string_codes(String,X),
	X = [H | Xs],
	check_first_char(H),
	check_rest_list(Xs).


%%% check_first_char/1
%%% True if the atom (H) is a csymf type.

check_first_char(H) :-
	code_type(H,csymf).


%%% check_rest_list/1
%%% True if the list contains the number code for a valid LabelName.
%%% An empty list is valid.

check_rest_list([]).


%%% check_rest_list/1
%%% Checks if all the atoms in the list are a csym type.

check_rest_list([H | T]) :-
	code_type(H,csym),
	check_rest_list(T).


%%% not_instruction/1
%%% True if the string value is different from the instruction names with value
%%% and from the single instruction name.

not_instruction(String) :-
	String \= "add",
	String \= "sub",
	String \= "sta",
	String \= "lda",
	String \= "bra",
	String \= "brz",
	String \= "brp",
	String \= "dat",
	not_single_instruction(String).


%%% not_single_instruction/1
%%% True if the string value is different from the single instruction name.

not_single_instruction(String) :-
	String \= "inp",
	String \= "out",
	String \= "hlt",
	String \= "dat".


%%% is_single_instruction/1
%%% True if the string value is equal to a single instruction name.
%%% case String is the instruction inp.

is_single_instruction(String) :-
	String = "inp",
	!.


%%% is_single_instruction/1
%%% case String is the instruction out.

is_single_instruction(String) :-
	String = "out",
	!.


%%% is_single_instruction/1
%%% case String is the instruction hlt.

is_single_instruction(String) :-
	String = "hlt",
	!.


%%% is_single_instruction/1
%%% case String is the instruction dat.

is_single_instruction(String) :-
	String = "dat",
	!.


%%% is_instruction_with_value/1
%%% True if the string value is equal to a instruction with value name
%%% case String is the instruction add.

is_instruction_with_value(String) :-
	String = "add",
	!.


%%% is_instruction_with_value/1
%%% case String is the instruction sub.

is_instruction_with_value(String) :-
	String = "sub",
	!.


%%% is_instruction_with_value/1
%%% case String is the instruction sta.

is_instruction_with_value(String) :-
	String = "sta",
	!.


%%% is_instruction_with_value/1
%%% case String is the instruction lda.

is_instruction_with_value(String) :-
	String = "lda",
	!.


%%% is_instruction_with_value/1
%%% case String is the instruction bra.

is_instruction_with_value(String) :-
	String = "bra",
	!.


%%% is_instruction_with_value/1
%%% case String is the instruction brz.

is_instruction_with_value(String) :-
	String = "brz",
	!.


%%% is_instruction_with_value/1
%%% case String is the instruction brp.

is_instruction_with_value(String) :-
	String = "brp",
	!.


%%% is_instruction_with_value/1
%%% case String is the instruction dat.

is_instruction_with_value(String) :-
	String = "dat",
	!.


%%% execution_loop/2
%%% If the memory length is not 100, the predicate fails with an error
%%% message (also the whole program fails).

execution_loop(State, _) :-
	arg(3, State, Mem),
	length(Mem, L),
	L \= 100,
	print_bold("Invalid memory length"),
	!,
	fail.


%%% execution_loop/2
%%% If the State is a state (and not an halted state), the program execution
%%% continues with the next instruction.

execution_loop(State, Out) :-
	State = state(_, _, _, _, _, _),
	!,
	one_instruction(State, NewState),
	execution_loop(NewState, Out).


%%% execution_loop/2
%%% If the State is an halted state, the program ends and prints the output
%%% list (Out).

execution_loop(State, Out) :-
	arg(5, State, Out).


%%% one_instruction/2
%%% Reads the current instruction from the memory and executes it.

one_instruction(State, NewState) :-
	arg(1, State, Acc),
	arg(2, State, Pc),
	arg(3, State, Mem),
	arg(4, State, In),
	arg(5, State, Out),
	arg(6, State, Flag),
	nth0(Pc, Mem, Value, _),
	FirstDigit is truncate(Value / 100),
	CellNum is mod(Value, 100),
	select_instruction(FirstDigit,
			   CellNum,
			   Acc,
			   Pc,
			   Mem,
			   In,
			   Out,
			   Flag,
			   NewState).


%%% select_instruction/9
%%% Selects and executes the correct operation.
%%% If the first digit (FirstDigit) of the instruction is 0, the instruction
%%% is an halt operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 0,
	!,
	halt_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% If the first digit (FirstDigit) of the instruction is 1, the instruction
%%% is an addoperation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 1,
	!,
	add_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% If the first digit (FirstDigit) of the instruction is 2, the instruction
%%% is a sub operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 2,
	!,
	sub_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% If the first digit (FirstDigit) of the instruction is 3, the instruction
%%% is a store operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 3,
	!,
	store_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% If the first digit (FirstDigit) of the instruction is 5, the instruction
%%% is a load operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 5,
	!,
	load_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% If the first digit (FirstDigit) of the instruction is 6, the instruction
%%% is a branch operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 6,
	!,
	branch_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% If the first digit (FirstDigit) of the instruction is 7, the instruction
%%% is a branch if zero operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 7,
	!,
	branch_if_zero_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% If the first digit (FirstDigit) of the instruction is 8, the instruction
%%% is a branch if positive operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 8,
	!,
	branch_if_positive_operation(CellNumber,
				     Acc,
				     Pc,
				     Mem,
				     In,
				     Out,
				     Flag,
				     NewState).


%%% select_instruction/9
%%% If the instruction number is 901 (FirstDigit = 9, CellNumber = 1), the
%%% instruction is an input operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 9,
	CellNumber = 1,
	!,
	input_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% If the instruction number is 902 (FirstDigit = 9, CellNumber = 2), the
%%% instruction is an output operation.

select_instruction(FirstDigit,
		   CellNumber,
		   Acc,
		   Pc,
		   Mem,
		   In,
		   Out,
		   Flag,
		   NewState) :-
	FirstDigit = 9,
	CellNumber = 2,
	!,
	output_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState).


%%% select_instruction/9
%%% Otherwise, the instruction is an illegal instruction operation.
%%% Ranges of illegal_instruction_operation: [400-499], 900, [903-999].

select_instruction(_, _, _, _, _, _, _, _, _) :-
	illegal_instruction_operation(_, _, _, _, _, _, _, _).


%%% add_operation/8
%%% Sums the accumulator value (Acc) with the memory cell content (CellContent)
%%% at CellNumber position and writes the result (mod 1000) in the accumulator.
%%% Updates the program counter (Pc) and creates the new LMC state.
%%% If the result is more than 0 and less than 1000, the flag value
%%% is "noflag"; otherwise the flag value is "flag".

add_operation(CellNumber, Acc, Pc, Mem, In, Out, _, NewState) :-
	nth0(CellNumber, Mem, CellContent, _),
	update_acc(Acc, CellContent, Acc2, Flag2),
	update_pc(Pc, Pc2),
	NewState =.. [state, Acc2, Pc2, Mem, In, Out, Flag2].


%%% sub_operation/8
%%% Subtracts the memory cell content (CellContent) at CellNumber position from
%%% the accumulator value (Acc) and writes the result (mod 1000) in the
%%% accumulator.
%%% Updates the program counter (Pc) and creates the new LMC state.
%%% If the result is more than 0 and less than 1000, the flag value is
%%% "noflag"; otherwise the flag value is "flag".

sub_operation(CellNumber, Acc, Pc, Mem, In, Out, _, NewState) :-
	nth0(CellNumber, Mem, CellContent, _),
	CellContent2 is (0 - CellContent),
	update_acc(Acc, CellContent2, Acc2, Flag2),
	update_pc(Pc, Pc2),
	NewState =.. [state, Acc2, Pc2, Mem, In, Out, Flag2].


%%% store_operation/8
%%% Saves the accumulator value(Acc) in the memory cell at CellNumber position.
%%% The accumulator content (Acc) remains unchanged.
%%% Updates the program counter (Pc) and creates the new LMC state.

store_operation(CellNumber, Acc, Pc, Mem, In, Out, Flag, NewState) :-
	replace(CellNumber, Mem, Acc, NewMem),
	update_pc(Pc, Pc2),
	NewState =.. [state, Acc, Pc2, NewMem, In, Out, Flag].


%%% load_operation/8
%%% Writes the memory cell content (CellContent) at CellNumber position
%%% in the accumulator (Acc). The accumulator (Acc) content remains unchanged.
%%% Updates the program counter (Pc) and creates the new LMC state.

load_operation(CellNumber, _, Pc, Mem, In, Out, Flag, NewState) :-
	nth0(CellNumber, Mem, CellContent, _),
	Acc is CellContent,
	update_pc(Pc, Pc2),
	NewState =.. [state, Acc, Pc2, Mem, In, Out, Flag].


%%% branch_operation/8
%%% Unconditional jump: sets the program counter value (Pc) to CellNumber.
%%% Updates the program counter (Pc) and creates the new LMC state.

branch_operation(CellNumber, Acc, _, Mem, In, Out, Flag, NewState) :-
	Pc is CellNumber,
	NewState =.. [state, Acc, Pc, Mem, In, Out, Flag].


%%% branch_if_zero_operation/8
%%% Conditional jump: if the accumulator content (Acc) is 0 and the flag value
%%% is "noflag", sets the program counter value (Pc) to CellNumber and creates
%%% the new LMC state.

branch_if_zero_operation(CellNumber, Acc, _, Mem, In, Out, Flag, NewState) :-
	Acc = 0,
	Flag = noflag,
	!,
	Pc2 is CellNumber,
	NewState =.. [state, Acc, Pc2, Mem, In, Out, Flag].


%%% branch_if_zero_operation/8
%%% Conditional jump: if the accumulator content (Acc) is not 0 or the flag
%%% value is "flag", updates the program counter value (Pc) and creates the
%%% new LMC state.

branch_if_zero_operation(_, Acc, Pc, Mem, In, Out, Flag, NewState) :-
	update_pc(Pc, Pc2),
	NewState =.. [state, Acc, Pc2, Mem, In, Out, Flag].


%%% branch_if_positive_operation/8
%%% Conditional jump: if the flag value is "noflag", sets the program counter
%%% value (Pc) to CellNumber and creates the new LMC state.

branch_if_positive_operation(CellNumber,
			     Acc,
			     _,
			     Mem,
			     In,
			     Out,
			     Flag,
			     NewState) :-
	Flag = noflag,
	!,
	Pc2 is CellNumber,
	NewState =.. [state, Acc, Pc2, Mem, In, Out, Flag].


%%% branch_if_positive_operation/8
%%% Conditional jump: if the flag value is "flag", updates the program counter
%%% value (Pc) and creates the new LMC state.

branch_if_positive_operation(_, Acc, Pc, Mem, In, Out, Flag, NewState) :-
	update_pc(Pc, Pc2),
	NewState =.. [state, Acc, Pc2, Mem, In, Out, Flag].


%%% input_operation/8
%%% If the input list (In) is not empty, and the first element (First) of
%%% the input list is in the range [0, 999] writes it in the accumulator
%%% value (Acc), removing it from the list. Updates the program counter
%%% (Pc) and creates the new LMC state.

input_operation(_, _, Pc, Mem, In, Out, Flag, NewState) :-
	check_input(In, First),
	Acc2 is First,
	delete_first_element(In, NewIn),
	update_pc(Pc, Pc2),
	NewState =.. [state, Acc2, Pc2, Mem, NewIn, Out, Flag].


%%% check_input/2
%%% If the input list (In) is empty, the predicate fails with an error
%%% message (also the whole program fails).

check_input(In, _) :-
	length(In, L),
	L = 0,
	print_bold('ERROR: Input list is empty'),
	!,
	fail.


%%% check_input/2
%%% If the first element (First) of the input list (In) is in the range
%%% [0, 999], returns the first element (First).

check_input(In, First) :-
	first_input_element(In, First),
	First >= 0,
	First =< 999,
	!.


%%% check_input/2
%%% If the first element (First) of the input list (In) is not in the
%%% range [0, 999], the predicate fails with an error message (also the
%%% whole program fails).

check_input(_, _) :-
	print_bold('ERROR: Invalid input list'),
	fail.


%%% output_operation/8
%%% Writes the accumulator value (Acc) at the end of the output list (Out).
%%% The accumulator content (Acc) remains unchanged.
%%% Updates the program counter (Pc) and creates the new LMC state.

output_operation(_, Acc, Pc, Mem, In, Out, Flag, NewState) :-
	append(Out, [Acc], NewOut),
	update_pc(Pc, Pc2),
	NewState =.. [state, Acc, Pc2, Mem, In, NewOut, Flag].


%%% halt_operation/8
%%% Creates the LMC halted state.

halt_operation( _, Acc, Pc, Mem, In, Out, Flag, NewState) :-
	NewState =.. [halted_state, Acc, Pc, Mem, In, Out, Flag].


%%% illegal_instruction_operation/8
%%% The predicate fails with an error message (also the whole program
%%% fails).

illegal_instruction_operation(_, _, _, _, _, _, _, _) :-
	print_bold("ERROR: ILLEGAL INSTRUCTION"),
	fail.


%%% update_pc/2
%%% Updates the program counter (Pc) by increasing it by 1 (mod 100).

update_pc(Pc, PcNew) :-
	Pc2 is Pc + 1,
	PcNew is mod(Pc2, 100).


%%% update_acc/4
%%% Sums the accumulator value (Acc) with the memory cell content (CellContent)
%%% and writes the result (mod 1000) in a new accumulator (NewAcc). If the
%%% result is less than 999 and more than 0, the flag value is "noflag".

update_acc(Acc, CellContent, NewAcc, Flag2) :-
	AccTemp is (Acc + CellContent),
	AccTemp =< 999,
	AccTemp >= 0,
	!,
	NewAcc is mod(AccTemp, 1000),
	Flag2 = noflag.


%%% update_acc/4
%%% Sums the accumulator value (Acc) with the memory cell content (CellContent)
%%% and writes the result (mod 1000) in a new accumulator (NewAcc). If the
%%% result is more than 999 or less than 0, the flag value is "flag".

update_acc(Acc, CellContent, NewAcc, Flag2) :-
	AccTemp is (Acc + CellContent),
	NewAcc is mod(AccTemp, 1000),
	Flag2 = flag.


%%% print_bold/1
%%% Prints the string passed as parameter.

print_bold(String) :-
	nl,
	ansi_format([bold, fg(red)], String, []).


%%% first_input_element/2
%%% Retuns the first element of the list.

first_input_element([H | _], H).


%%% delete_first_element/2
%%% Deletes the first element of the list.

delete_first_element([_ | T], T).


%%% replace/4
%%% Replaces the element at position (Pos) in the list (List) with the element
%%% Elem and returns the list with the replaced element.

replace(Pos, List, Elem, NewList) :-
	nth0(Pos, List, _, TempList),
	nth0(Pos, NewList, Elem, TempList).


%%%% end of file -- LMC.pl
