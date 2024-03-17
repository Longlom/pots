-module(rss_queue).
-include("/usr/lib/erlang/lib/xmerl-1.3.28/include/xmerl.hrl").
-include("/usr/lib/erlang/lib/inets-7.5/include/httpd.hrl").

-export([start/0, add_item/2, add_feed/2, get_all/1, server/1]).

-define(TIMEOUT, 1000).

start() ->
    Pid = spawn(?MODULE, server, [[]]),
    {ok, Pid}.

add_item(QPid, Item) when is_pid(QPid) ->
    QPid ! {add_item, Item},
    ok.

add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
    Items = rss_parse:get_feed_items(RSS2Feed, []),
    lists:foreach(fun(Item) -> add_item(QPid, Item) end, Items),
    ok.

get_all(QPid) when is_pid(QPid) ->
    QPid ! {get_all, self()},
    receive
        {all_items, Items} -> {ok, Items}
    after ?TIMEOUT ->
        {error, timeout}
    end.

server(Queue) ->
    receive
        {add_item, RSSItem} ->
            NewQueue = update_queue(Queue, RSSItem),
            server(NewQueue);
        {get_all, ReqPid} ->
            ReqPid ! {all_items, Queue},
            server(Queue)
    end.

update_queue(Queue, RSSItem) ->
    NewQueue =
        case find_item(Queue, RSSItem) of
            % Элемент уже есть, игнорируем
            {same, _} ->
                Queue;
            {updated, OldItem} ->
                % Удаляем старый элемент, добавляем новый, сортируем
                lists:sort(
                    fun(Item1, Item2) ->
                        rss_parse:compare_feed_items(Item1, RSSItem) == updated orelse
                            rss_parse:compare_feed_items(Item1, Item2) == less
                    end,
                    [RSSItem | lists:delete(OldItem, Queue)]
                );
            different ->
                % Просто добавляем новый элемент, сортируем
                lists:sort(
                    fun(Item1, Item2) ->
                        rss_parse:compare_feed_items(Item1, Item2) == less
                    end,
                    [RSSItem | Queue]
                )
        end,
    NewQueue.

find_item(Queue, RSSItem) ->
    lists:foldl(
        fun(Item, Acc) ->
            case rss_parse:compare_feed_items(Item, RSSItem) of
                same -> {same, Item};
                updated -> {updated, Item};
                different -> Acc
            end
        end,
        different,
        Queue
    ).
