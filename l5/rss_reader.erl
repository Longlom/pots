-module(rss_reader).
-export([start/2]).
-define(RETRIEVE_INTERVAL, 60000). % Интервал времени в миллисекундах (60 секунд)

start(Url, QPid) ->
    spawn(?MODULE, server, [Url, QPid]).

server(Url, QPid) ->
    loop(Url, QPid).

loop(Url, QPid) ->
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _, Body}} ->
            case xmerl_scan:string(Body) of
                {ok, ParsedContent} ->
                    case rss_parse:is_rss2_feed(ParsedContent) of
                        true ->
                            rss_queue:add_feed(ParsedContent, QPid), % Предполагается, что эта функция добавляет ленту в очередь
                            ok;
                        false ->
                            % Если контент не соответствует RSS 2.0, ничего не делаем
                            ok
                    end;
                _Error ->
                    % Обработка ошибок разбора XML
                    ok
            end;
        _HttpResponse ->
            % Обработка других HTTP-ответов или ошибок запроса
            ok
    end,
    receive
        after ?RETRIEVE_INTERVAL -> ok
    end,
    loop(Url, QPid). % Рекурсивно вызываем loop для повторения операций
