

-module(erl_test_machine).
-author("David Leon").
-define(FAILING_GRADE,60).
-define(FULL_GRADE,100).
-define(DECREASING_ON_WARNING,0).
-define(DECREASING_POINTS_VAL,5).%how much points to decrease for each mistake
-define(EXCERCISE_STRING,"ex9").
-define(GRADES_FILE_NAME,"Grades_Sheet_"++?EXCERCISE_STRING++".csv").
-record(student, {id,module_name,grade,compile_status,warning=warning_passed}).
-export([init/0]).

init()-> 
	file:delete(?GRADES_FILE_NAME),
	CurrentDir = element(2,file:get_cwd()),
	FilesList = filter_by_erl_ending(element(2,file:list_dir(CurrentDir)),[]),
	ListOfStudents = compile_files(FilesList,[]),
	ListOfStudentsWithGrades = testing_loop(ListOfStudents,[]),
	print_results_to_csv_file(ListOfStudentsWithGrades).

compile_files([],ListOfStudents)-> ListOfStudents; 
compile_files(FilesList,ListOfStudents)-> SeekID = string:find(hd(FilesList),?EXCERCISE_STRING++"_",leading),
	case SeekID of 
	nomatch ->  compile_files(tl(FilesList),ListOfStudents);
	_ ->
		IDTmp = tl(string:find(hd(FilesList),"_",leading)),
		ID = hd(lists:filter(fun(X) -> X /= [] end,string:replace(IDTmp,string:find(IDTmp,"."),""))),
		case compile:file(hd(FilesList),[return_warnings]) of
			{ok,ModuleName,[]}-> compile_files(tl(FilesList),
							ListOfStudents++[#student{id=ID,module_name=ModuleName,grade=?FULL_GRADE,compile_status=compilation_passed}]);%compilation failed;
			{ok,ModuleName,_Warning}-> compile_files(tl(FilesList),
							ListOfStudents++[#student{id=ID,module_name=ModuleName,grade=?FULL_GRADE-?DECREASING_ON_WARNING,compile_status=compilation_passed,warning=warning_failed}]);%compilation failed;
			error -> compile_files(tl(FilesList),
							 ListOfStudents++[#student{id=ID,module_name=hd(FilesList),grade=?FAILING_GRADE,compile_status=compilation_failed}])
		end
	end.

print_results_to_csv_file([])->done;
print_results_to_csv_file(ListOfStudents)-> Student=hd(ListOfStudents),
	file:write_file(?GRADES_FILE_NAME,io_lib:fwrite("~s,~p,~p,~p,~n",[Student#student.id,Student#student.grade,Student#student.compile_status,Student#student.warning]),[append]),
	print_results_to_csv_file(tl(ListOfStudents)).

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
		compilation_failed-> testing_loop(tl(ListOfStudents),UpdatedList++[CurrentRecord]),io:format("Student: ~s failed ~n",[CurrentRecord#student.module_name]);
		compilation_passed-> io:format("Testin Student: ~s ~n",[CurrentRecord#student.module_name]),
				     Grade = max(CurrentRecord#student.grade-testing(CurrentRecord#student.module_name),?FAILING_GRADE),
				     NewRecord = CurrentRecord#student{grade=Grade},
				     testing_loop(tl(ListOfStudents),UpdatedList++[NewRecord])
	end.


testing(ModuleName) -> ListOfTests = test_block(ModuleName),
		       ListOfTestsFailed = lists:filter(fun(X)->X/=true end,lists:flatten(ListOfTests)),
		       length(ListOfTestsFailed)*?DECREASING_POINTS_VAL.%Amount of failures * Grade decreasing value

%write here list of tests protected by try and catch
test_block(X)->[
	[try X:findKelem([],1) =:= notFound of Res->Res catch _:_->failed end ],
	[try X:union([1,2,3,4],[5,6,7,3]) =:= [1,2,3,4,5,6,7] of Res->Res catch _:_->failed end]
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%		Supportin methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%for complex programs which might stuck, after 100ms skip to next check
%Warning this is an unsafe method of try catch after, This implementation is recommended for tests only. 
%example of using running envelope: [running_envelope(fun()-> ModuleName:FuncX(Input) end)]
running_envelope(Fun)->
Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() -> Parent ! {ok, Fun()} end),
    receive
        {ok, _ExprRes} ->
            erlang:demonitor(Ref, [flush]),
            true;
        {'DOWN', Ref, process, Pid, _Info} ->
            failed
        after 100-> 
        	failed
    end.


%%%%%%%%%%%% files operations %%%%%%%%%%%%%
compare_two_files(FILENAME_A,FILENAME_B)->
	ListOfLinesA = readlines(FILENAME_A),
	ListOfLinesB = readlines(FILENAME_B),
	lists:sort(ListOfLinesA) =:= lists:sort(ListOfLinesB).

readlines(FileName) ->
    case file:open(FileName, [read]) of 
    	{ok, Device} ->
		    try get_all_lines(Device)
		      after file:close(Device)
		    end;
		    _-> []
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.
