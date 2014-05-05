%%%----------------------------------------------------------------------
%%%
%%% @author chenduo
%%% @date 2013-11-13
%%% @doc lists的扩展
%%%
%%%----------------------------------------------------------------------
-module(listsex).
-export([for/2, foracc/3, while/2, while/3, untilerror/2]).
-export([find/2, mapfind/2, foldlwhile/3]).

-export([classify/2, classify/4]).
-export([count/2, fcount/2, takecount/1]).
-export([indexof/2, findexof/2, insertat/3, removeat/2, takeat/2, replaceat/3]).
-export([insertsorted/2, insertsortedf/3]).
-export([keycount/3, keyfoldl/5, keyfoldr/5]).
-export([keyclassify/2, keyclassify/4, keyclassifyl/2, keyclassifyl/4]).

-export([randallot/2, randallot2/2, shuffle/1, shuffle2/1]).
-export([randstuff/2, randstuff_withlimit/3, randstuff_withlimit/4]).
-export([rand/1, rand/2, rand_pick/1, rand_pick/2]).
-export([pick_by_weight/1, pick_by_prob/1]).
-export([drawrand/1, drawrand/2, drawrand_index/1, drawrand_index/2]).
-export([rand_dissimilar_from_deeplist/1]).

-export([priority_accept/3, remove_duplicates/1, subtract_rmdup/2]).

%% @doc 执行指定函数N次
for(_Fun, N) when N =< 0 ->
    ok;
for(Fun, N) ->
    Fun(),
    for(Fun, N - 1).

%% @doc 执行指定函数N次并fold结果
foracc(_, 0, Acc) ->
    Acc;
foracc(Fun, N, Acc) ->
    foracc(Fun, N-1, Fun(Acc)).

%% @doc 执行指定函数直至条件不满足
while(F, Acc) ->
    case F(Acc) of
        false ->
            Acc;
        Acc2 ->
            while(F, Acc2)
    end.

%% @doc 执行指定函数直至条件不满足
while(Pred, F, Acc) ->
    while(Pred, F, F(Acc), Acc).
while(Pred, F, Next, Acc) ->
    case Pred(Next) of
        true ->
            while(Pred, F, F(Next), Next);
        false ->
            Acc
    end.

%% @doc 执行指定函数直至出错
untilerror(Fun, Acc) ->
    case catch Fun(Acc) of
        {error, Reason} ->
            {Reason, Acc};
        Acc2 ->
            untilerror(Fun, Acc2)
    end.

%% @doc 返回第一个匹配的元素
find(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            H;
        false ->
            find(Pred, T)
    end;
find(_, []) ->
    false.

%% @doc 返回第一个执行指定函数后结果不为false的结果
mapfind(Fun, [H|T]) ->
    case Fun(H) of
        false ->
            mapfind(Fun, T);
        Val ->
            Val
    end;
mapfind(_, []) ->
    false.

%% @doc 数个数
count(E, L) ->
    count(E, L, 0).
count(_E, [], Acc) ->
    Acc;
count(E, [E|T], Acc) ->
    count(E, T, Acc + 1);
count(E, [_|T], Acc) ->
    count(E, T, Acc).

%% @doc 数个数
fcount(F, L) ->
    fcount(F, L, 0).
fcount(_F, [], Acc) ->
    Acc;
fcount(F, [H|T], Acc) ->
    case F(H) of
        true ->
            fcount(F, T, Acc + 1);
        false ->
            fcount(F, T, Acc)
    end.

%% @doc 统计列表各元素个数
%% e.g [1, 2, 3, 4, 2, 1, 3] -> [{1,2},{2,2},{3,2},{4,1}]
takecount(L) ->
	takecount(L, []).
takecount([], Acc) ->
	Acc;
takecount([H|T], Acc) ->
	case lists:keytake(H, 1, Acc) of
		false ->
			takecount(T, [{H, 1} | Acc]);
		{value, {H, N}, Acc2} ->
			takecount(T, [{H, N + 1} | Acc2])
	end.

%%---------------------------------------------------------------
%% 位置相关
%%---------------------------------------------------------------

%% @doc 返回给定元素在列表中的位置,从1开始
indexof(Ele, L) ->
    indexof(Ele, L, 1).
indexof(Ele, [Ele|_T], Idx) ->
    Idx;
indexof(Ele, [_|T], Idx) ->
    indexof(Ele, T, Idx + 1);
indexof(_, [], _) ->
    false.

%% @doc 返回符合给定条件的元素在列表中的位置,从1开始
findexof(Fun, L) when is_function(Fun, 1) ->
    findexof(Fun, L, 1).
findexof(Fun, [H|T], Idx) ->
    case Fun(H) of
        true ->
            Idx;
        false ->
            findexof(Fun, T, Idx + 1)
    end;
findexof(_, [], _) ->
    false.

%% @doc 向list中指定位置插入元素
insertat(E, Idx, L) ->
    insertat(E, Idx, L, [], 1).
insertat(_, _, [], Acc, _) ->
    lists:reverse(Acc);
insertat(E, Idx, [_|T], Acc, Count) when Count =:= Idx ->
    lists:reverse([E|Acc], T);
insertat(E, Idx, [H|T], Acc, Count) ->
    insertat(E, Idx, T, [H|Acc], Count+1).

%% @doc 从list中删除指定位置的元素
removeat(Idx, L) ->
    removeat(Idx, L, [], 1).
removeat(_, [], Acc, _) ->
    lists:reverse(Acc);
removeat(Idx, [_|T], Acc, Count) when Count =:= Idx ->
    lists:reverse(Acc, T);
removeat(Idx, [H|T], Acc, Count) ->
    removeat(Idx, T, [H|Acc], Count+1).

%% @doc 从list中删除指定位置的元素,返回删除后的列表和删掉的元素
takeat(Idx, L) ->
    takeat(Idx, L, [], 1).
takeat(_, [], Acc, _) ->
    {false, lists:reverse(Acc)};
takeat(Idx, [H|T], Acc, Count) when Count =:= Idx ->
    {H, lists:reverse(Acc, T)};
takeat(Idx, [H|T], Acc, Count) ->
    takeat(Idx, T, [H|Acc], Count+1).

%% @doc 替换list中指定位置的元素
replaceat(Idx, L, Val) ->
	replaceat(Idx, L, [], 1, Val).
replaceat(_, [], Acc, _, _) ->
	lists:reverse(Acc);
replaceat(Idx, [_|T], Acc, Count, Val) when Count =:= Idx ->
	replaceat(Idx, T, [Val|Acc], Count+1, Val);
replaceat(Idx, [H|T], Acc, Count, Val) ->
	replaceat(Idx, T, [H|Acc], Count+1, Val).

%%-------------------------------------------------------------
%% 顺序相关
%%-------------------------------------------------------------

%% @doc 将元素插入一个列表,要求两个参数均有序,这样结果仍然保持有序
insertsorted(NewGoods, L) ->
    insertsorted(NewGoods, L, [], 1, []).
insertsorted([], T, Acc, _, Counts) ->
    {lists:reverse(Acc, T), lists:reverse(Counts, [])};
insertsorted([NH|NT], [H|_T]=L, Acc, Count, Counts) when NH < H ->
    insertsorted(NT, L, [NH|Acc], Count+1, [Count|Counts]);
insertsorted(Adds, [H|T], Acc, Count, Counts) ->
    insertsorted(Adds, T, [H|Acc], Count+1, Counts).

%% @doc 将元素插入一个列表,要求两个参数均有序,这样结果仍然保持有序
%%      返回值是新列表和插入的几个元素被插入的位置列表
%% @end
insertsortedf(Pred, NewGoods, L) ->
    insertsortedf(Pred, NewGoods, L, [], 1, []).
insertsortedf(_, NewGoods, [], Acc, Count, Counts) ->
    {lists:reverse(Acc, NewGoods), lists:reverse(Counts, lists:seq(Count, Count+length(NewGoods)-1))};
insertsortedf(_, [], T, Acc, _, Counts) ->
    {lists:reverse(Acc, T), lists:reverse(Counts, [])};
insertsortedf(Pred, [NH|NT] = Adds, [H|T]=L, Acc, Count, Counts) ->
    case Pred(NH, H) of
        true ->
            insertsortedf(Pred, NT, L, [NH|Acc], Count+1, [Count|Counts]);
        false ->
            insertsortedf(Pred, Adds, T, [H|Acc], Count+1, Counts)
    end.

%% @doc 直到Pred返回false之前一直fold
foldlwhile(Pred, Acc0, [H|T]) ->
    case Pred(H, Acc0) of
        {true, Acc} ->
            foldlwhile(Pred, Acc, T);
        {false, Acc} ->
            Acc
    end;
foldlwhile(_Pred, Acc, []) ->
    Acc.

%% @doc 将一个列表分成几个子列表
%%      e.g. [{a,1},{b,1},{a,2},{b,3},{c,1}] -> [{c,[1]},{b,[1,3]},{a,[1,2]}]
%% @end
classify(FC, L) ->
    classify(FC, L, [], fun(E, Acc) -> [E|Acc] end). 
classify(_, [], _, _) ->
    [];
classify(FC, L, Acc0, FA) ->
    lists:foldr(fun(Ele, Acc) ->
        Key = FC(Ele),
        case lists:keytake(Key, 1, Acc) of
            false ->
                [{Key, FA(Ele, Acc0)}|Acc];
            {value, {Key, OldVal}, T} ->
                [{Key, FA(Ele, OldVal)}|T]
        end
    end, [], L).

%% @doc 以Tuple中的第N个元素为Key,将一个列表分成几个子列表
%%      e.g. [{a,1},{b,1},{a,2},{b,3},{c,1}] -> [{c,[1]},{b,[1,3]},{a,[1,2]}]
%% @end
keyclassify(N, L) ->
    keyclassify(N, L, [], fun(E, Acc) -> [E|Acc] end). 
keyclassify(_, [], _, _) ->
    [];
keyclassify(N, L, Acc0, F) ->
    lists:foldr(fun(Ele, Acc) ->
        Key = element(N, Ele),
        case lists:keytake(Key, 1, Acc) of
            false ->
                [{Key, F(Ele, Acc0)}|Acc];
            {value, {Key, List}, NewAcc} ->
                [{Key, F(Ele, List)}|NewAcc]
        end
    end, [], L).

%% @doc 与keyclassify的差别只在于顺序会倒过来,元素数量大(百万条以上?)时效率较高
%%      也许这个函数没有什么用
%% @end
keyclassifyl(N, L) ->
    keyclassifyl(N, L, [], fun(E, Acc) -> [E|Acc] end). 
keyclassifyl(_, [], _, _) ->
    [];
keyclassifyl(N, L, Acc0, F) ->
    lists:foldl(fun(Ele, Acc) ->
        Key = element(N, Ele),
        case lists:keytake(Key, 1, Acc) of
            false ->
                [{Key, F(Ele, Acc0)}|Acc];
            {value, {Key, List}, NewAcc} ->
                [{Key, F(Ele, List)}|NewAcc]
        end
    end, [], L).

%% @doc 按Key数个数
keycount(Key, N, L) when is_integer(N), N > 0 ->
    keycount(Key, N, L, 0).
keycount(_Key, _N, [], Acc) ->
    Acc;
keycount(Key, N, [H|T], Acc) when element(N, H) =:= Key ->
    keycount(Key, N, T, Acc + 1);
keycount(Key, N, [_|T], Acc) ->
    keycount(Key, N, T, Acc).

%% @doc 按Key做fold
keyfoldl(_, _, _, Acc, []) ->
    Acc;
keyfoldl(Key, N, F, Acc, [H|T]) when element(N, H) =:= Key ->
    keyfoldl(Key, N, F, F(H, Acc), T);
keyfoldl(Key, N, F, Acc, [_|T]) ->
    keyfoldl(Key, N, F, Acc, T).

%% @doc 按Key做fold, 从右向左
keyfoldr(_, _, _, Acc, []) ->
    Acc;
keyfoldr(Key, N, F, Acc, [H|T]) when element(N, H) =:= Key ->
    F(H, keyfoldr(Key, N, F, Acc, T));
keyfoldr(Key, N, F, Acc, [_|T]) ->
    keyfoldr(Key, N, F, Acc, T).

%%----------------------------------------------------------------------------
%% 优先级条件取子列表
%%----------------------------------------------------------------------------

%% @doc 按优先级过滤列表,要求得到N个元素;
%%      当满足所有条件的元素不足N个时,降格以求,去掉第一个条件继续检索;
%%      仍不足时,继续降格以求,但最后一个条件必须满足;
%%      得到N个元素时返回 
%%      一个很重要的考虑是,List可能很大,发现N个时立即返回以满足性能要求,不要对List做全遍历
%% @end
priority_accept(L, FilterList, N) ->
    priority_accept(L, FilterList, N, []).
priority_accept([], _, _, Acc) -> % 只有这些了
    Acc;
priority_accept(_, [], _, Acc) -> % 只能这样了,不能再降格以求了
    Acc;
priority_accept(_, _, 0, Acc) -> % 找齐了
    Acc;
priority_accept(L, [_|T] = FilterList, N, Acc) ->
    {Left, GotN, Got} = take(L, N, FilterList),
    priority_accept(Left, T, N - GotN, Got ++ Acc).

%% @doc 从列表里取符合所有条件的N个元素
take(L, TargetN, FilterList) ->
    take(L, TargetN, FilterList, 0, [], []).
take([], _, _, AccN, Acc, LeftAcc) -> % 只有这些
    {LeftAcc, AccN, Acc};
take(_, TargetN, _, TargetN, Acc, LeftAcc) -> % 找齐了
    {LeftAcc, TargetN, Acc};
take([H|T], TargetN, FilterList, AccN, Acc, LeftAcc) ->
    case multi_filter(H, FilterList) of
        true ->
            take(T, TargetN, FilterList, AccN + 1, [H|Acc], LeftAcc);
        false ->
            take(T, TargetN, FilterList, AccN, Acc, [H|LeftAcc])
    end.

%% @doc 判断一个元素是否符合一个条件列表
multi_filter(Item, FilterList) ->
    lists:all(fun(Filter) -> Filter(Item) end, FilterList).

%% 没有想到这个实现还没有上面的实现效率高, 差了一倍
% %% @doc 按优先级过滤列表,要求得到N个元素;
% %%      当满足所有条件的元素不足N个时,降格以求,去掉第一个条件继续检索;
% %%      仍不足时,继续降格以求,但最后一个条件必须满足;
% %%      得到N个元素时返回 
% %% @end
% priority_accept2(L, FilterList, Must, N) ->
%     priority_accept2(L, FilterList, Must, N, []).
% priority_accept2([], _, _, _, Acc) -> % 只有这些了
%     Acc;
% priority_accept2(_, _, _, 0, Acc) -> % 找齐了
%     Acc;
% priority_accept2(L, [], Must,  N, Acc) -> % 只能这样了,不能再降格以求了
%     {_, _, Got} = take2(L, N, [], Must),
%     Got ++ Acc;
% priority_accept2(L, [_|T] = FilterList, Must, N, Acc) ->
%     {Left, GotN, Got} = take2(L, N, FilterList, Must),
%     priority_accept2(Left, T, Must, N - GotN, Got ++ Acc).
% 
% %% @doc 从列表里取符合所有条件的N个元素
% take2(L, TargetN, FilterList, Must) ->
%     take2(L, TargetN, FilterList, Must, 0, [], []).
% take2([], _, _, _, AccN, Acc, LeftAcc) -> % 只有这些
%     {LeftAcc, AccN, Acc};
% take2(_, TargetN, _, _, TargetN, Acc, LeftAcc) -> % 找齐了
%     {LeftAcc, TargetN, Acc};
% take2([H|T], TargetN, FilterList, Must, AccN, Acc, LeftAcc) ->
%     case multi_filter2(H, FilterList, Must) of
%         true ->
%             take2(T, TargetN, FilterList, Must, AccN + 1, [H|Acc], LeftAcc);
%         false ->
%             take2(T, TargetN, FilterList, Must, AccN, Acc, [H|LeftAcc]);
%         dead ->
%             take2(T, TargetN, FilterList, Must, AccN, Acc, LeftAcc)
%     end.
% 
% %% @doc 判断一个元素是否符合一个条件列表
% multi_filter2(Item, FilterList, Must) ->
%     case Must(Item) of
%         false ->
%             dead;
%         true ->
%             lists:all(fun(Filter) -> Filter(Item) end, FilterList)
%     end.

%% @doc 去除重复的元素
remove_duplicates(L) ->
    sets:to_list(sets:from_list(L)).

%% @doc 减集并去重, 不保持原顺序
%%      e.g. ([3,4,5,6], [1,2,3,3,4]) -> [6,5]
%%           ([1,2,3,3,4], [3,4,5,6]) -> [2,1] 
subtract_rmdup(L1, L2) ->
    sets:to_list(sets:subtract(sets:from_list(L1), sets:from_list(L2))).

%%----------------------------------------------------------------------------
%% 随机相关
%%----------------------------------------------------------------------------

%% @doc 将数随机地分配给列表中的元素
%%      e.g: 5, [a,b,c] -> [{a,1},{b,4}]
%% @end
randallot(Val, L) ->
    N = length(L),
    randallot(Val, L, N, lists:zip(L, lists:duplicate(N, 0))).
randallot(0, _, _, Acc) ->
    Acc;
randallot(Val, L, N, Acc) ->
    Key = lists:nth(random:uniform(N), L),
    {_, OldVal} = lists:keyfind(Key, 1, Acc),
    Acc2 = lists:keyreplace(Key, 1, Acc, {Key, OldVal+1}),
    randallot(Val-1, L, N, Acc2).

%% @doc 将数随机地分配给列表中的元素, 另一种算法
%%      e.g: 5, [a,b,c] -> [{a,1},{b,2},{c,2}]
%% @end
randallot2(_, []) ->
    [];
randallot2(Val, [H]) ->
    [{H,Val}];
randallot2(0, L) ->
    N = length(L),
    lists:zip(L, lists:duplicate(N, 0));
randallot2(Val, [H|T]) ->
    V = random:uniform(Val+1)-1, % 直接用uniform范围是[1-Val],这样写的范围是[0-Val]
    [{H, V}|randallot(Val - V, T)].

%% @doc 随机打乱一个列表
shuffle(L) ->
    List1 = [{random:uniform(), X} || X <- L], 
    List2 = lists:keysort(1, List1), 
    [E || {_, E} <- List2]. 

%% @doc 另一种算法,随机打乱一个列表
shuffle2(L) -> 
    shuffle2(L, []).
shuffle2([], Acc) -> Acc;
shuffle2(L, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(L)) - 1, L),
    shuffle2(Leading ++ T, [H | Acc]).

%% @doc 随机填充,即从候选列表中随机取数得到一个新列表,
%%      Size表示目标列表的长度
%% @end
randstuff(Candidates, Size) when Size >= 0  ->
    randstuff(Candidates, length(Candidates), Size, []).
randstuff(_, _, 0, Acc) ->
    Acc;
randstuff(Candidates, N, Size, Acc) ->
    randstuff(Candidates, N, Size - 1, [lists:nth(random:uniform(N), Candidates)|Acc]).

%% @doc 随机填充,即从候选列表中随机取数得到一个新列表,
%%      Size表示目标列表的长度,
%%      Limit表示每个候选值能出现的最大次数,
%%      Size和Limit冲突时以Limit为准
%% @end
randstuff_withlimit(Candidates, Size, Limit, Tail) ->
    L1 = randstuff_withlimit(Candidates, Size, Limit),
    L = if length(Tail) > Size -> L1 ++ Tail; true -> Tail ++ L1 end,
    shuffle(L).
randstuff_withlimit(Candidates, Size, Limit) when Size >= 0, Size =< Limit ->
    % Size =< Limit 此时Limit无效
    randstuff(Candidates, Size);
randstuff_withlimit(Candidates, Size, Limit) when Size >= 0, Size >= length(Candidates) * Limit ->
    % 此情况下把所有能出的值出完也不够Size指定的数, 也就不用随机了 
    shuffle(lists:append([lists:duplicate(Limit, C) || C <- Candidates]));
randstuff_withlimit(Candidates, Size, Limit) when Size >= 0, is_list(Candidates) ->
    randstuff_withlimit(Candidates, length(Candidates), Size, Limit, [], []).
randstuff_withlimit(_, _, 0, _, _, Acc) ->
    % 达到目标Size
    Acc;
randstuff_withlimit(_, 0, _, _, _, Acc) ->
    % Candidates耗尽
    Acc;
randstuff_withlimit(Candidates, N, Size, Limit, AccLimit, Acc) ->
    C = lists:nth(random:uniform(N), Candidates),
    {NewCount, AccLimit2} = case lists:keytake(C, 1, AccLimit) of
        false ->
            {1, AccLimit};
        {value, {C, Count}, AccLimitTemp} ->
            {Count + 1, AccLimitTemp}
    end,
    case NewCount >= Limit of
        true ->
            % 已达到Limit, 去除该候选值
            randstuff_withlimit(Candidates -- [C], N - 1, Size - 1, Limit, [{C, NewCount}|AccLimit2], [C|Acc]);
        false ->
            randstuff_withlimit(Candidates, N, Size - 1, Limit, [{C, NewCount}|AccLimit2], [C|Acc])
    end.

%% @doc 从附有概率的列表中抽取几个值,每得到一个值即将其从列表中移除再进行下一次抽取,使用相对概率
%%      例如: drawrand([{a, 1000}, {b, 2000}, {c, 500}, {d, 4000}], 2) -> [b, d]
%% @end
drawrand(L) ->
    drawrand(L, 1).
drawrand(L, Round) ->
    drawrand(L, Round, []).
drawrand([], _, Acc) -> % 牌抽完了,没得抽了
    Acc;
drawrand(_, 0, Acc) -> % 抽够了,不抽了
    Acc;
drawrand(L, Round, Acc) ->
	{Obj, Pos} = pick_by_weight(L),
    L2 = removeat(Pos, L),
	drawrand(L2, Round - 1, [Obj | Acc]).

%% @doc 例: drawrand_index([3000,2000,300,100,50], 3) -> [1,2,4]
drawrand_index(L) ->
    drawrand_index(L, 1).
drawrand_index(L, Round) ->
    L2 = lists:zip(lists:seq(1, length(L)), L),
    drawrand(L2, Round).

%% @doc 返回0到某个数,或两个数之间的随机数
rand(0) -> 0;
rand(N) when N > 0 ->
    random:uniform(N).
rand(Min, Min)->
    Min;
rand(Min, Max) when Min < Max ->
    rand(Max - Min + 1) + Min - 1.

%% @doc 在列表中返回1个
rand_pick([_|_] = List) ->
    Pos = rand(length(List)),
    lists:nth(Pos, List);
rand_pick(Contents) when is_tuple(Contents) ->
    element(rand:uniform(tuple_size(Contents)), Contents).

%% @doc 在列表中随机返回N个
rand_pick(L, N) ->
    rand_pick(L, N, []).
rand_pick(_, 0, Result) -> % 已得到N个值
    Result;
rand_pick([], _, Result) -> % 遍历结束,直接返回Result
    Result;
rand_pick(L, N, Result) ->
    Pos = rand(erlang:length(L)),
    Val = lists:nth(Pos, L),
    rand_pick(L--[Val], N-1, [Val|Result]).

%% @doc 依据权值选择
%% 列表格式:[{对象, 权值}]
%% 返回{对象,位置}
%% 每项的概率为  权值/权值总和
pick_by_weight(L) ->
    pick_by_prob(L, lists:sum([W || {_, W} <- L])).

%% @doc 依据概率选择, 以10000为基准
%% 列表格式:[{对象,概率}]
%% 返回{对象,位置}
pick_by_prob(L) -> 
    pick_by_prob(L, 10000).
pick_by_prob(L, W) ->
    Point = rand(W),
    pick_by_prob(L, 0, Point, 1).
pick_by_prob([], _, _, _)->
    false;
pick_by_prob([{Obj, Range} | _], Cur, Point, Index) when Cur < Point andalso Point =< (Range + Cur) ->
    {Obj, Index};
pick_by_prob([{_, Range} | Rest], Cur, Point, Index) ->
    pick_by_prob(Rest, Range + Cur, Point, Index + 1).

%% @doc 对每个元素随机选择
%% e.g. ([[{411002,6000},{411003, 4000}],[{411001, 3000}, {411002, 3000}, {411006, 4000}]]) -> [411002, 411006]
%% @end
rand_dissimilar_from_deeplist(L) when is_list(L) ->
    rand_dissimilar_from_deeplist(L, []);
rand_dissimilar_from_deeplist(L) when is_tuple(L) ->
    rand_dissimilar_from_deeplist(tuple_to_list(L), []).
rand_dissimilar_from_deeplist([], Acc) ->
    Acc;
rand_dissimilar_from_deeplist([H|T], Acc) ->
    case lists:foldl(fun(Id, R) -> lists:keydelete(Id, 1, R) end, H, Acc) of
        [] ->
            rand_dissimilar_from_deeplist(T, Acc);
        L ->
            {Id, _} = pick_by_weight(L),
            rand_dissimilar_from_deeplist(T, [Id | Acc])
    end.

