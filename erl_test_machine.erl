

-module(erl_test_machine).
-author("David Leon").
-define(FAILING_GRADE,60).
-define(FULL_GRADE,100).
-define(DECREASING_POINTS_VAL,3).%how much points to decrease for each mistake 
-record(student, {id,module_name,grade,compile_status}).
-export([init/0]).

init()-> 
	CurrentDir = element(2,file:get_cwd()),
	FilesList = filter_by_erl_ending(element(2,file:list_dir(CurrentDir)),[]),
	ListOfStudents = compile_files(FilesList,[]),
	testing_loop(ListOfStudents,[]).

compile_files([],ListOfStudents)-> ListOfStudents; 
compile_files(FilesList,ListOfStudents)-> SeekID = string:find(hd(FilesList),"_",leading),
	case SeekID of 
	nomatch ->  compile_files(tl(FilesList),ListOfStudents);
	_ ->
		IDtmp = tl(string:find(hd(FilesList),"_",leading)),
		ID = hd(lists:filter(fun(X) -> X /= [] end,string:replace(IDtmp,string:find(IDtmp,"."),""))),
		ModuleName = element(2,compile:file(hd(FilesList))),
		case ModuleName of 
		error -> compile_files(tl(FilesList),
				ListOfStudents++[#student{id=ID,module_name=hd(FilesList),grade=?FAILING_GRADE,compile_status=compilation_failed}]);
		_ -> compile_files(tl(FilesList),
				ListOfStudents++[#student{id=ID,module_name=ModuleName,grade=?FULL_GRADE,compile_status=compilation_passed}])%compilation failed 
		end
	end.
				
				

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


testing(ModuleName) ->
	io:format("~p~n", [ModuleName:findKelem([],1) =:= notFound]) ,
	io:format("~p~n", [ModuleName:findKelem([1,2,5,7],5) =:= notFound]),
	io:format("~p~n", [ModuleName:findKelem([4],1) =:= 4]),
	io:format("~p~n", [ModuleName:findKelem([1,2,7,5,8,6],3) =:= 7]),
	io:format("~p~n", [ModuleName:findKelem([{1,a},{2,b},{3,c},{4,d}],2) =:= {2,b}]),

	io:format("~p~n", [ModuleName:reverse([]) =:= []]),
	io:format("~p~n", [ModuleName:reverse([{b,4}]) =:= [{b,4}]]),
	io:format("~p~n", [ModuleName:reverse([4,5]) =:= [5,4]]),
	io:format("~p~n", [ModuleName:reverse([1,a,2,b,c,3,e]) =:= [e,3,c,b,2,a,1]]),
	io:format("~p~n", [ModuleName:reverse([1,1,1,1]) =:= [1,1,1,1]]),
	io:format("~p~n", [ModuleName:reverse([{1,a},{2,b},{3,c},{4,d},2,[5,3],r]) =:= [r,[5,3],2,{4,d},{3,c},{2,b},{1,a}]]),

	io:format("~p~n", [ModuleName:deleteKelem([1,2.0,{a,4.0}],{a,4}) =:= [1,2.0,{a,4.0}]]),
	io:format("~p~n", [ModuleName:deleteKelem([],3) =:= []]),
	io:format("~p~n", [ModuleName:deleteKelem([1,2,3,4,3,4],3) =:= [1,2,4,4]]),
	io:format("~p~n", [ModuleName:deleteKelem([{a,1},{b,2},[1,2,3],{c,3},{d,4},b,a,[1,2,3],{a,1},[1,2,3]],[1,2,3]) =:= [{a,1},{b,2},{c,3},{d,4},b,a,{a,1}]]),
	io:format("~p~n", [ModuleName:deleteKelem([{a,1},{b,2},[1,2,3],{c,3},{d,4},b,a,[1,2,3],{a,1},[1,2,3]],2) =:= [{a,1},{b,2},[1,2,3],{c,3},{d,4},b,a,[1,2,3],{a,1},[1,2,3]]]),
	io:format("~p~n", [ModuleName:deleteKelem([1,1,1,1,1],1) =:= []]),
	
	io:format("~p~n", [ModuleName:addKelem([1,2],1,4) =:= [4,1,2]]),
	io:format("~p~n", [ModuleName:addKelem([],1,8) =:= [8]]),
	io:format("~p~n", [ModuleName:addKelem([3,4],3,5) =:= [3,4,5]]),
	io:format("~p~n", [ModuleName:addKelem([{a,2},{b,4},c,d,[1,2,3]],4,[1,{a,4},3]) =:= [{a,2},{b,4},c,[1,{a,4},3],d,[1,2,3]]]),
	io:format("~p~n", [ModuleName:addKelem([1,2,3,4,5],2,{a,8}) =:= [1,{a,8},2,3,4,5]]),
	io:format("~p~n", [ModuleName:addKelem([1,2,3],4,[]) =:= [1,2,3,[]]]),

	io:format("~p~n", [ModuleName:union([],[{a,1},{b,2},[4,5,6]]) =:= [{a,1},{b,2},[4,5,6]]]),
	io:format("~p~n", [ModuleName:union([],[]) =:= []]),
	io:format("~p~n", [ModuleName:union([{a,1},{b,2},[4,5,6]],[]) =:= [{a,1},{b,2},[4,5,6]]]),
	io:format("~p~n", [ModuleName:union([{a,1},{q,2},{a,b,c},[8,10,12]],[{a,1},{q,2},{a,b,c},[8,10,12]]) =:= [{a,1},{q,2},{a,b,c},[8,10,12]]]),
	io:format("~p~n", [ModuleName:union([[1,2,3,4,5],[1,2,3,4,5]],[{m,4},[]]) =:= [[1,2,3,4,5],{m,4},[]]]),
	io:format("~p~n", [ModuleName:union([1,{a},{b},[f,2]],[3,a,1.0,{b}]) =:= [1,{a},{b},[f,2],3,a,1.0]]),
	io:format("~p~n", [ModuleName:union([1,2,3,4],[5,6,7,3]) =:= [1,2,3,4,5,6,7]]).
