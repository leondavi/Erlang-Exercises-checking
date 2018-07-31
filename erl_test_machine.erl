

-module(erl_test_machine).
-author("David Leon").
-define(FAILING_GRADE,60).
-define(FULL_GRADE,100).
-define(DECREASING_POINTS_VAL,3).%how much points to decrease for each mistake
-define(EXCERCISE_STRING,"ex2").
-define(GRADES_FILE_NAME,"grades.txt").
-record(student, {id,module_name,grade,compile_status}).
-export([init/0]).

init()-> 
	CurrentDir = element(2,file:get_cwd()),
	FilesList = filter_by_erl_ending(element(2,file:list_dir(CurrentDir)),[]),
	ListOfStudents = compile_files(FilesList,[]),
	ListOfStudentsWithGrades = testing_loop(ListOfStudents,[]),
	print_results_to_text_file(ListOfStudentsWithGrades).

compile_files([],ListOfStudents)-> ListOfStudents;
compile_files(FilesList,ListOfStudents)-> SeekID = string:find(hd(FilesList),?EXCERCISE_STRING++"_",leading),
	case SeekID of
		nomatch ->  compile_files(tl(FilesList),ListOfStudents);
		_ ->
			IDTmp = tl(string:find(hd(FilesList),"_",leading)),
			ID = hd(lists:filter(fun(X) -> X /= [] end,string:replace(IDTmp,string:find(IDTmp,"."),""))),
			%io:format("~p~n", [compile:file(hd(FilesList),[return_warnings])]),
			case compile:file(hd(FilesList),[return_warnings]) of
				{ok,ModuleName}-> compile_files(tl(FilesList),
					ListOfStudents++[#student{id=ID,module_name=ModuleName,grade=?FULL_GRADE,compile_status=compilation_passed}]);%compilation failed;
				{ok,ModuleName,_Warning}-> compile_files(tl(FilesList),
					ListOfStudents++[#student{id=ID,module_name=ModuleName,grade=?FULL_GRADE-?DECREASING_ON_WARNING,compile_status=compilation_passed,warning=warning_failed}]);%compilation failed;
				error -> compile_files(tl(FilesList),
					ListOfStudents++[#student{id=ID,module_name=hd(FilesList),grade=?FAILING_GRADE,compile_status=compilation_failed,warning=warning_failed}])
			end
	end.

print_results_to_text_file([])->done;
print_results_to_text_file(ListOfStudents)-> Student=hd(ListOfStudents),
	file:write_file(?GRADES_FILE_NAME,io_lib:fwrite("~s    |   ~p    |    ~p~n",[Student#student.id,Student#student.grade,Student#student.compile_status]),[append]),
	print_results_to_text_file(tl(ListOfStudents)).

%filtering file names, keeping .erl files
filter_by_erl_ending([],NewList)-> NewList;
filter_by_erl_ending(List,NewList)-> case string:find(hd(List),".erl",trailing) of
			".erl" -> filter_by_erl_ending(tl(List),NewList++[hd(List)]);
			nomatch -> filter_by_erl_ending(tl(List),NewList)
			end.	
%This loop is running the testing process over all compiled excercises
testing_loop([],UpdatedList) -> UpdatedList;
testing_loop(ListOfStudents,UpdatedList) -> CurrentRecord = hd(ListOfStudents),
	case CurrentRecord#student.compile_status of
		compilation_failed-> testing_loop(tl(ListOfStudents),UpdatedList++[CurrentRecord]);
		compilation_passed-> Grade = testing(CurrentRecord#student.module_name),
				     NewRecord = CurrentRecord#student{grade=Grade},
				     testing_loop(tl(ListOfStudents),UpdatedList++[NewRecord])
	end.


testing(ModuleName) -> ListOfTests = [
	[try ModuleName:findKelem([],1) =:= notFound of Res->Res catch _:_->failed end ],
	[try ModuleName:findKelem([1,2,5,7],5) =:= notFound of Res->Res catch _:_->failed end],
	[try ModuleName:findKelem([4],1) =:= 4  of Res->Res catch _:_->failed end],
	[try ModuleName:findKelem([1,2,7,5,8,6],3) =:= 7 of Res->Res catch _:_->failed end],
	[try ModuleName:findKelem([{1,a},{2,b},{3,c},{4,d}],2) =:= {2,b}  of Res->Res catch _:_->failed end],

	[try ModuleName:reverse([]) =:= [] of Res->Res catch _:_->failed end],
	[try ModuleName:reverse([{b,4}]) =:= [{b,4}] of Res->Res catch _:_->failed end],
	[try ModuleName:reverse([4,5]) =:= [5,4]of Res->Res catch _:_->failed end],
	[try ModuleName:reverse([1,a,2,b,c,3,e]) =:= [e,3,c,b,2,a,1]of Res->Res catch _:_->failed end],
	[try ModuleName:reverse([1,1,1,1]) =:= [1,1,1,1]of Res->Res catch _:_->failed end],
	[try ModuleName:reverse([{1,a},{2,b},{3,c},{4,d},2,[5,3],r]) =:= [r,[5,3],2,{4,d},{3,c},{2,b},{1,a}] of Res->Res catch _:_->failed end],

	[try ModuleName:deleteKelem([1,2.0,{a,4.0}],{a,4}) =:= [1,2.0,{a,4.0}] of Res->Res catch _:_->failed end],
	[try ModuleName:deleteKelem([],3) =:= [] of Res->Res catch _:_->failed end],
	[try ModuleName:deleteKelem([1,2,3,4,3,4],3) =:= [1,2,4,4] of Res->Res catch _:_->failed end],
	[try ModuleName:deleteKelem([{a,1},{b,2},[1,2,3],{c,3},{d,4},b,a,[1,2,3],{a,1},[1,2,3]],[1,2,3]) =:= [{a,1},{b,2},{c,3},{d,4},b,a,{a,1}] of Res->Res catch _:_->failed end],
	[try ModuleName:deleteKelem([{a,1},{b,2},[1,2,3],{c,3},{d,4},b,a,[1,2,3],{a,1},[1,2,3]],2) =:= [{a,1},{b,2},[1,2,3],{c,3},{d,4},b,a,[1,2,3],{a,1},[1,2,3]] of Res->Res catch _:_->failed end],
	[try ModuleName:deleteKelem([1,1,1,1,1],1) =:= [] of Res->Res catch _:_->failed end],
	
	[try ModuleName:addKelem([1,2],1,4) =:= [4,1,2] of Res->Res catch _:_->failed end],
	[try ModuleName:addKelem([],1,8) =:= [8] of Res->Res catch _:_->failed end],
	[try ModuleName:addKelem([3,4],3,5) =:= [3,4,5] of Res->Res catch _:_->failed end],
	[try ModuleName:addKelem([{a,2},{b,4},c,d,[1,2,3]],4,[1,{a,4},3]) =:= [{a,2},{b,4},c,[1,{a,4},3],d,[1,2,3]] of Res->Res catch _:_->failed end],
	[try ModuleName:addKelem([1,2,3,4,5],2,{a,8}) =:= [1,{a,8},2,3,4,5] of Res->Res catch _:_->failed end],
	[try ModuleName:addKelem([1,2,3],4,[]) =:= [1,2,3,[]] of Res->Res catch _:_->failed end],

	[try ModuleName:union([],[{a,1},{b,2},[4,5,6]]) =:= [{a,1},{b,2},[4,5,6]] of Res->Res catch _:_->failed end],
	[try ModuleName:union([],[]) =:= [] of Res->Res catch _:_->failed end],
	[try ModuleName:union([{a,1},{b,2},[4,5,6]],[]) =:= [{a,1},{b,2},[4,5,6]] of Res->Res catch _:_->failed end],
	[try ModuleName:union([{a,1},{q,2},{a,b,c},[8,10,12]],[{a,1},{q,2},{a,b,c},[8,10,12]]) =:= [{a,1},{q,2},{a,b,c},[8,10,12]] of Res->Res catch _:_->failed end],
	[try ModuleName:union([[1,2,3,4,5],[1,2,3,4,5]],[{m,4},[]]) =:= [[1,2,3,4,5],{m,4},[]] of Res->Res catch _:_->failed end],
	[try ModuleName:union([1,{a},{b},[f,2]],[3,a,1.0,{b}]) =:= [1,{a},{b},[f,2],3,a,1.0] of Res->Res catch _:_->failed end],
	[try ModuleName:union([1,2,3,4],[5,6,7,3]) =:= [1,2,3,4,5,6,7] of Res->Res catch _:_->failed end]
] ,ListOfTestsFailed = lists:filter(fun(X)->X/=true end,lists:flatten(ListOfTests)),ListOfTestsFailed,?FULL_GRADE-length(ListOfTestsFailed)*?DECREASING_POINTS_VAL
.
