%%% $Id: tws.erl,v 1.15 2010/05/16 13:33:57 micro Exp $
%%% Fri April 27 18:23:47 GMT 2010 -primus  µ
%%% File:     tws.erl
%%  @author   R. Primus  <rprimus@gmail.com>
%%    [http://www.soi.city.ac.uk/~bm119]
%%  @doc      toy program to connect to IB TWSs API and display data.
%%  Ultimate goal is to create .scid files for use with SierraChart

%% @reference <a href="http://oreilly.com/catalog/9780596518189/">Erlang Programming</a>,
%%  <em> Francesco Cesarini and Simon Thompson</em>,
%%  O'Reilly, 2009.
%% @reference <a href="http://pragprog.com/titles/jaerlang/programming-erlang"> Programming Erlang</a>
%%  <em> Joe Armstrong</em>,
%%  Pragmatic Bookshelf, 2007.
%% @copyright (c) © 2010, R Primus. All Rights Reserved
%%

%% @headerfile "tws.hrl"

%% Type definitions and comments for edoc.

%% Created:  Wed Apr 27 18:23:47 2010 by R. Primus  <rprimus@gmail.com>
%%


%%% THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, EXPRESS,
%%% IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF
%%% MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

%%% IN NO EVENT SHALL R Primus BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR
%%% CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE POSSIBILITY OF
%%% DAMAGE, AND ON ANY THEORY OF LIABILITY,  ARISING OUT OF OR IN CONNECTION WITH
%%% THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% See the file COPYING
%%%

-module(tws).
-export([start/0, start/2, stop/0]).
-export([connect/1, set_loglevel/1, req_mktdata/2, cancel_mktdata/1]).
-export([req_real_time_bars/2, cancel_real_time_bars/1, req_contract_data/2, req_contract_data/4, set_contract/0, set_contract/3]).
-export([loop/1, parse_data/1]).
-export([asciz/1]).
-export([get_contracts/0, get_contracts/1]).
-export([start_real_time/2]).

-vsn('1.0.0').
-author("R, Primus").
-copyright("Copyright (c) 2010, R Primus. All Rights Reserved").
-doc("").
-purpose("").

-include_lib("tws/include/tws.hrl").

-define(TCP_OPTIONS, [inet, {packet, 0}, binary]).
-define(TWS_HOST, "localhost").
-define(CONTRACTS_FILE, "contracts.dat").
-define(FL(X), list_to_float(X)).
-define(TIME_FMT, "~2..0B:~2..0B:~2..0B").
-define(TIME(H,M,S), H,M,S).

start() ->
  start(?TWS_HOST, 666).
start(Host, ClientID) ->
  {ok, Socket} = gen_tcp:connect(Host, ?PORT, ?TCP_OPTIONS),
  Pid = spawn(?MODULE,loop,[Socket]),
  register(?MODULE, Pid),
  gen_tcp:controlling_process(Socket, Pid),
  connect(ClientID),
  {ok, Pid}.

connect(ClientID) ->
  call(?CLIENT_VERSION),
  call(ClientID),
  ok.

set_loglevel(Loglevel) ->
  call(?SET_SERVER_LOGLEVEL),
  call(1),
  call(Loglevel),
  ok.

get_contracts() ->
  get_contracts(?CONTRACTS_FILE).
get_contracts(File) ->
  case file:consult(File) of
    {ok, Contracts} ->
      create_dict(Contracts);
    Error -> {File, Error}
  end.

create_dict(Contracts) ->
  Db = dict:new(),
  create_dict(Contracts, Db).

create_dict([], Db) -> Db;
create_dict([H|T], Db) ->
  Db1 = dict:store((H#contract_details.summary)#contract.symbol, H, Db),
  create_dict(T, Db1).

start_real_time([], _) -> ok;
start_real_time([H|T], Acc) ->
  spawn(tws, req_real_time_bars, [Acc, H]),
  start_real_time(T, Acc+1).

req_contract_data(TickerID, Contract) ->
  call(?REQ_CONTRACT_DATA),
  call(6),
  call(TickerID),
  call("0"),
  call(Contract#contract.symbol),
  call(Contract#contract.sectype),
  call(Contract#contract.expiry),
  call(Contract#contract.strike),
  call(Contract#contract.right),
  call(Contract#contract.multiplier),
  call(Contract#contract.exchange),
  call(Contract#contract.currency),
  call(""),
  call(0),
  call(""),
  call(""),    %snapshot (false)
  ok.
req_contract_data(TickerID, Symbol, Exchange, Expiry) ->
  req_contract_data(TickerID, set_contract(Symbol, Exchange, Expiry)).

req_mktdata(TickerID, Contract) ->
  call(?REQ_MKT_DATA),
  call(8),
  call(TickerID),
  call(Contract#contract.symbol),
  call(Contract#contract.sectype),
  call(Contract#contract.expiry),
  call(Contract#contract.strike),
  call(Contract#contract.right),
  call(Contract#contract.multiplier),
  call(Contract#contract.exchange),
  call(""),
  call(Contract#contract.currency),
  call(""),
  call(0),
  call(""),
  call(0),    %snapshot (false)
  ok.

cancel_mktdata(TickerID) ->
  call(?CANCEL_MKT_DATA),
  call(2),
  call(TickerID),
  ok.

req_real_time_bars(TickerID, Contract) ->
  call(?REQ_REAL_TIME_BARS),
  call(1),
  call(TickerID),
  call(Contract#contract.symbol),
  call(Contract#contract.sectype),
  call(Contract#contract.expiry),
  call(Contract#contract.strike),
  call(Contract#contract.right),
  call(Contract#contract.multiplier),
  call(Contract#contract.exchange),
  call(""),
  call(Contract#contract.currency),
  call(""),
  call(5),
  call("TRADES"),
  call(0),    %use RTH
  ok.

cancel_real_time_bars(TickerID) ->
  call(?CANCEL_REAL_TIME_BARS),
  call(1),
  call(TickerID),
  ok.

set_contract() ->
  #contract{}.
set_contract(Symbol, Expiry, Exchange) ->
  #contract{symbol=Symbol, expiry=Expiry, exchange=Exchange}.


stop() ->
    call(stop),
    ok.

call(Command) ->
    ?MODULE ! {send, Command}.

loop(Socket) ->
  %process_flag(trap_exit, true),
  receive
    {send, Command} ->
      case Command of
        stop ->
          gen_tcp:close(Socket),
          {Socket, closed};
        _ ->
          gen_tcp:send(Socket, asciz(Command)),
          loop(Socket)
      end;
    {tcp, Socket, Bin}  ->
      spawn(tws, parse_data, [string:tokens(binary_to_list(Bin), "\0")]),
      loop(Socket);
    {tcp_closed, Socket} ->
      gen_tcp:close(Socket)
    %{'EXIT', Pid, Reason} ->
    %    {exit, Pid, Reason}
  end.
parse_data([]) -> ok;

parse_data(L) ->
  [MsgID|Rest] = L,
  Size=length(L),
  case list_to_integer(MsgID) of
    ?SERVER_VERSION ->
      [SerDateTime| Data] = Rest,
      io:format("Connected at: ~p~n", [SerDateTime]),
      parse_data(Data);
    ?NEXT_VALID_ID ->
      [_Ver, ID| Data] = Rest,
      io:format("Next OrderID: ~p~n", [ID]),
      parse_data(Data);
    ?OPEN_ORDER_END ->
      [_Ver| Data] = Rest,
      parse_data(Data);
    ?CONTRACT_DATA_END ->
      [_Ver, _TickerID| Data] = Rest,
      parse_data(Data);
    ?TICK_PRICE ->
      [_Ver, TickerID, TickTypeInt, TickPrice, TickSize, _CanAutoExecute|Data] = Rest,
      io:format("tick_price: ticker: ~p ", [TickerID]),
      io:format("type: ~p ", [get_tick_type(list_to_integer(TickTypeInt))]),
      io:format("price: ~p , size: ~p~n", [TickPrice, TickSize]),
      parse_data(Data);
    ?TICK_SIZE ->
      [_Ver, TickerID, TickTypeInt, TickSize |Data] = Rest,
      io:format("tick_size: ticker: ~p ", [TickerID]),
      io:format("type: ~p ", [get_tick_type(list_to_integer(TickTypeInt))]),
      io:format("size: ~p~n", [TickSize]),
      parse_data(Data);
    ?TICK_GENERIC ->
      [_Ver, TickerID, TickTypeInt, Value |Data] = Rest,
      io:format("tick_generic: ticker: ~p ", [TickerID]),
      io:format("type: ~p ", [get_tick_type(list_to_integer(TickTypeInt))]),
      io:format("Value: ~p~n", [Value]),
      parse_data(Data);
    ?TICK_STRING ->
      [_Ver, TickerID, TickTypeInt, Value |Data] = Rest,
      io:format("tick_string: ticker: ~p ", [TickerID]),
      io:format("type: ~p ", [get_tick_type(list_to_integer(TickTypeInt))]),
      io:format("String: ~p~n", [Value]),
      parse_data(Data);
    ?ERR_MSG ->
      [_Ver, Id, ErrorCode, ErrorMsg| Data] = Rest,
      io:format("error_msg: Id: ~p: Code: ~p Msg: ~p~n", [Id, ErrorCode, ErrorMsg]),
      parse_data(Data);
    ?CONTRACT_DATA ->
      %io:format("contract_data [~p] ~p~n", [Size, Rest]),
      [
      _Ver, _ReqId, Symbol, SecType, Expiry, Strike, Exchange, Currency, LocalSymbol, MarketName, TradingClass, ConId, MinTick, Multiplier, OrderTypes, ValidExchanges, PriceMagnifier, UnderConId, LongName, ContractMonth, TimeZoneId, TradingHours, LiquidHours| Data] = Rest,
      C = #contract_details{
          summary = #contract{symbol=Symbol,
            sectype=SecType,
            expiry=Expiry,
            strike=Strike,
            exchange=Exchange,
            currency=Currency,
            local_symbol=LocalSymbol},
        marketName=MarketName,
        tradingClass=TradingClass,
        conId=ConId,
        minTick=MinTick,
        multiplier=Multiplier,
        orderTypes=OrderTypes,
        validExchanges=ValidExchanges,
        priceMagnifier=PriceMagnifier,
        underConId=UnderConId,
        longName=LongName,
        contractMonth=ContractMonth,
        timeZoneId=TimeZoneId,
        tradingHours=TradingHours,
        liquidHours=LiquidHours},
      case file:open(?CONTRACTS_FILE, [append]) of
        {ok, S} ->
          io:format(S, "~p.~n", [C]),
          file:close(S);
        Error -> {?CONTRACTS_FILE, Error}
      end,
      io:format("~p~n", [C]),
      parse_data(Data);
    ?NEWS_BULLETINS ->
      io:format("news_bulletins [~p] ~p~n", [Size, Rest]);
    ?HISTORICAL_DATA ->
      io:format("historical_data [~p] ~p~n", [Size, Rest]);
    ?CURRENT_TIME ->
      io:format("current_time [~p] ~p~n", [Size, Rest]);
    ?REAL_TIME_BARS ->
      [_Ver, ReqId, Time, Open, High, Low, Close, Volume, Average, Count| Data] = Rest,
      {_, {H, M, S}} = calendar:gregorian_seconds_to_datetime(list_to_integer(Time)),
      io:format("ticker: ~s; " ++ ?TIME_FMT ++  " ~.3f, ~.3f, ~.3f, ~.3f, ~4s, [~.3f, ~4s] ~8.3f~n",
          [ReqId, ?TIME(H,M,S), ?FL(Open), ?FL(High), ?FL(Low), ?FL(Close),
            Volume, ?FL(Average), Count, vol_per_trade(Volume, Count)]),
      parse_data(Data);
    _ ->
      io:format("~p~n", [L])
  end.
vol_per_trade(V, C) ->
  case list_to_integer(C) of
    0 ->  0;
    _ -> list_to_integer(V)/list_to_integer(C)
  end.
get_tick_type(T) ->
  {T, Type} = lists:keyfind(T, 1, ?TICK_TYPE),
  Type.
asciz(V) ->
  case is_integer(V) of
    true ->
      integer_to_list(V) ++ "\0";
    _ -> V ++ "\0"
  end.


%% vim: set et ts=2 sw=2 ai invlist si cul nu:
