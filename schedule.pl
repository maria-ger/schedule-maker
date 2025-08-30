% student(surname, time).
% teacher(surname, time, load).

% константы, ограничивающие число студентов в группе
group_size_min(3).
group_size_max(7).

% константа, задающая min число свободных дней между днями занятий группы
free_days(1).

% загрузка фактов из файла
load_file(File) :- see(File), repeat, read(F), (F = end_of_file, !, seen; assert(F), fail). 

% главный предикат - взаимодействие с пользователем
schedule() :- write('Enter name of file with data.\n'), 
              read(F), load_file(F),
              write('Enter a to see applications of students.\n'),
              write('Enter t to see info about teachers.\n'),
              write('Enter s to see generated schedule.\n'),
              write('Enter e to exit.\n'),
              repeat, read(C), 
              (C = 'e', retractall(student(_, _)), retractall(teacher(_, _, _)), !; 
               C = 'a', print_applications(), fail;
               C = 't', print_teacher_info(), fail;
               C = 's', print_schedule(), fail).

% получить список студентов, у которых в заявках одно время (Time)
time_pref([Time, S]) :- findall(X, student(X, Time), S).

% разделить студентов с одинаковым временем на группы (от 3 до 7 человек)
form_groups(_, [], []).
form_groups(S, G, All) :- flatten([S1, G], All), insertion_sort(S1, S).

form_groups([Time, L]) :- time_pref([Time, S]), 
                          setof(G, G^form_groups(_, G, S), [L|_]).

% формирование групп. результат: список с элементами [время, список студентов]
groups(G) :- findall(S, student(S, 'morning'), Sm), findall(S, student(S, 'evening'), Se), 
             length(Sm, Lm), length(Se, Le), group_size_min(Low), group_size_max(High),
             Mm1 is div(Lm, Low), Me1 is div(Le, Low), M1 is Mm1 + Me1,
             Mm2 is div(Lm, High), Me2 is div(Le, High), M2 is Mm2 + Me2,
             range(M2, M1, R), member(N, R), 
             form_groups(['morning', L1]), addtoall('morning', L1, G1), 
             form_groups(['evening', L2]), addtoall('evening', L2, G2), 
             delete_n_first(G1, G, G2), length(G, N).

% поиск сбалансированного распределения по группам
% с мин. разницей в количестве студентов в группах
find_min_groups(Res) :- findall(G, groups(G), L), max_size_dif(L, MS), min_list(MS, M),
                        find_min(L, MS, M, Res).

find_min([Gs], _, _, Gs).
find_min([Gs|_], [MS|_], M, Gs) :- MS = M, !.
find_min([_|T1], [_|T2], M, Res) :- find_min(T1, T2, M, Res).

% создание расписания для списка групп и списка учителей
% для группы выбираются два дня занятий и преподаватель, у которого они свободны
% тогда выбранные дни помечаются у преподавателя как занятые и загрузка уменьшается
create_schedule(_, _, [], _, []).
create_schedule(Num, Off, [[Time, G]|T1], Tchrs, 
                [[Tchr, [D1, D2], Time, N, G]|T2]) :- weekday(Day1, D1), weekday(Day2, D2), check_days(Day1, Day2), 
                                                      member(Tchr/T/Load, Tchrs), member(Time, T), Load > 1,
                                                      add_taken_day(Day1, Tchr, Off, Off1),  
                                                      add_taken_day(Day2, Tchr, Off1, Off2),
                                                      delete_one(Tchr/T/Load, Tchrs, Ts), L is Load - 2,
                                                      N is Num + 1, create_schedule(N, Off2, T1, [Tchr/T/L|Ts], T2).

create_schedule(G, S) :- init_taken_days(Off), findall(Surname/Time/Load, teacher(Surname, Time, Load), T), 
                         create_schedule(0, Off, G, T, S). 

% проверка, что между днями занятий группы есть хотя бы один свободный день
check_days(D1, D2) :- range(1, 7, D), member(D1, D), member(D2, D), free_days(N), D2 - D1 > N.

weekday(1, 'Monday').
weekday(2, 'Tuesday').
weekday(3, 'Wednesday').
weekday(4, 'Thursday').
weekday(5, 'Friday').
weekday(6, 'Saturday').
weekday(7, 'Sunday').

% добавить к дням, в которые у преподавателя уже есть занятия, еще оддин
add_taken_day(D, Tchr, [[L, Tchr]|T], [[[D|L], Tchr]|T]) :- not(member(D, L)), !.
add_taken_day(D, Tchr, [X|T], [X|T1]) :- add_taken_day(D, Tchr, T, T1).

% изначально у всех преподавателей все дни свободны, поэтому инициализация пустым списком
init_taken_days(Res) :- findall(S, teacher(S, _, _), Tchrs), addtoall([], Tchrs, Res).

% проверка существования расписания
not_exist() :- find_min_groups(G), create_schedule(G, _), !, fail.
not_exist() :- write('Schedule can\'t be created.\n').

% печать результатов (взаимодействие с пользователем)
% - напечатать распределение по группам, расписание для дня/недели
% - перейти к следующему варианту расписания
print_schedule() :- not(find_min_groups(_)), write('Groups can\'t be formed.\n'), !.
print_schedule() :- not_exist(), !.
print_schedule() :- find_min_groups(G), create_schedule(G, S), 
                    write('Enter g to see groups.\n'),
                    write('Enter \'<Weekday>\' to see day schedule.\n'), 
                    write('Enter w to see whole week schedule.\n'), 
                    write('Enter n to see another variant of schedule.\n'), 
                    write('Enter q to quit schedule mode.\n'), 
                    print(G, S, C), (C = 'q', write('Enter e to exit.\n'), !; fail).
print_schedule() :- write('You\'ve seen all schedule options.\n').

print(G, S, R) :- read(C), 
                  (C = 'q', R = C, !; 
                   C = 'g', print_groups(G, S), print(G, S, R);
                   member(C, ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']),
                   print_day_schedule(C, S), print(G, S, R);
                   C = 'w', range(1, 7, Week), print_whole_schedule(Week, S), print(G, S, R);
                   C = 'n', R = C, !).

% печать расписания на неделю
print_whole_schedule([], _).
print_whole_schedule([D|T], S) :- weekday(D, Day), print_day_schedule(Day, S), nl, print_whole_schedule(T, S).

% печать расписания на определенный день
print_day_schedule(Day, S) :- write('--'), write(Day), write('--'), nl, 
                              write('morning\n'), find_day(Day, 'morning', S), 
                              write('evening\n'), find_day(Day, 'evening', S).

% поиск групп, у которых есть занятия в заданный день
find_day(_, _, []) :- nl.
find_day(Day, Time, [[Tchr, Days, Time, N, Group]|T]) :- member(Day, Days), write('  '), write(N), write(' '), 
                                                         write(Tchr), write(' | '), 
                                                         make_ordered(Group, G), print_surnames(G, ' '),
                                                         find_day(Day, Time, T), !.
find_day(Day, Time, [_|T]) :- find_day(Day, Time, T).

% печать групп (группа: номер, преподаватель, список студентов по алфавиту)
print_groups([], []). 
print_groups([[_, Group]|Gs], [[Tchr, _, _, N, Group]|T]) :- write(N), write(' '), write(Tchr), nl, 
                                                             make_ordered(Group, G), print_surnames(G, '\n'), 
                                                             print_groups(Gs, T).

print_surnames([], _) :- nl.
print_surnames([S|T], Split) :- write(S), write(Split), print_surnames(T, Split).

% печать заявок студентов
print_applications() :- findall(S/T, student(S, T), List), write('student time\n'), print_data(List).

% печать информации о преподавателях
print_teacher_info() :- findall(S/T/L, teacher(S, T, L), List), write('teacher time load\n'), print_data(List).

print_data([]) :- nl.
print_data([Tchr/[_, _]/Load|T]) :- write(Tchr), write(' '), write('morning/evening'),  write(' '), write(Load), nl, print_data(T), !.
print_data([Tchr/[Time]/Load|T]) :- write(Tchr), write(' '), write(Time), write(' '), write(Load), nl, print_data(T), !.
print_data([St/Time|T]) :- write(St), write(' '), write(Time), nl, print_data(T).


% вспомогательные предикаты

range(N, N, [N]) :- !.
range(N1, N, [N1|T]) :- N2 is N1 + 1, range(N2, N, T).

addtoall(_, [], []).
addtoall(E, [X|T], [[E, X]|T1]) :- addtoall(E, T, T1).

insert(X, [], [X]).
insert(X, [Y|T], [X, Y|T]) :- X < Y.
insert(X, [Y|T], [Y|T1]) :- X >= Y, insert(X, T, T1).

insertion_sort([X], [X]).
insertion_sort([X|T], L) :- insertion_sort(T, L1), insert(X, L1, L), !.

delete_one(E, [E|T], T) :- !.
delete_one(E, [X|T1], [X|T2]) :- delete_one(E, T1, T2).

delete_n_first([], L, L).
delete_n_first([X|T], [X|T1], L) :- delete_n_first(T, T1, L).

flatten([[], []], []).
flatten([[Len|Lens], [G|T]], L) :- dif(G, []), delete_n_first(G, L, L1),
                                   group_size_min(Min), group_size_max(Max), 
                                   length(G, Len), Len >= Min, Len =< Max, 
                                   flatten([Lens, T], L1).

max_size_dif([], []).
max_size_dif([Gs|T], [M|Ms]) :- size_dif_list(Gs, S), max_list(S, M), max_size_dif(T, Ms).

size_dif_list([_], []).
size_dif_list([[_, G1]|Gs], L) :- length(G1, N1), size_dif(N1, Gs, L1),
                                  size_dif_list(Gs, L2), delete_n_first(L1, L, L2), !.

size_dif(_, [], []).
size_dif(N1, [[_, G2]|G], [N|Ns]) :- length(G2, N2), N is N2 - N1, 
                                     size_dif(N1, G, Ns).

strcmp([], _).
strcmp([X|T1], [X|T2]) :- strcmp(T1, T2).
strcmp([X|_], [Y|_]) :- X<Y, !.

insert_string(X, [], [X]).
insert_string(X, [Y|T], [X,Y|T]) :- string_codes(X, L1), string_codes(Y, L2), strcmp(L1, L2),!.
insert_string(X, [Y|T], [Y|T1]) :- insert_string(X, T, T1).

make_ordered([X], [X]).
make_ordered([X|T], L) :- make_ordered(T, L1), insert_string(X, L1, L).
