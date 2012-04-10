%%% $Id: scid.erl,v 1.1 2010/05/21 15:16:27 micro Exp $
%%% Fri Mar  5 18:31:35 GMT 2010 -primus �
%%% File:     scid.erl
%%% @author   R. Primus  <rprimus@gmail.com>
%%% @doc      ###description### 

%%% === System start and stop ===
%%% {@link start/0. `start/0'}, 
%%% {@link start/1. `start/1'}, 
%%% {@link stop/0. `stop/0'} 
%%% === Customer Service API ===
%%% {@link add_usr/3. `add_usr/3'},
%%% {@link delete_usr/1. `delete_usr/1'},
%%% {@link set_service/3. `set_service/3'},
%%% {@link set_status/2. `set_status/2'},
%%% {@link delete_disabled/0. `delete_disabled/0'},
%%% {@link lookup_id/1. `lookup_id/1'}
%%% === Service API ===
%%% {@link lookup_msisdn/1. `lookup_msisdn/1'},
%%% {@link service_flag/2. `service_flag/2'}


%%% @reference <a href="http://oreilly.com/catalog/9780596518189/">Erlang Programming</a>,
%%%  <em> Francesco Cesarini and Simon Thompson</em>,
%%%  O'Reilly, 2009.
%%% @reference <a href="http://pragprog.com/titles/jaerlang/programming-erlang"> Programming Erlang</a>
%%%  <em> Joe Armstrong</em>,
%%%  Pragmatic Bookshelf, 2007.
%%% @copyright (c) �  2010, R Primus. All Rights Reserved
%%%

%%% @headerfile "scid.hrl"

%%% Type definitions and comments for edoc.

%%% @type instTime() = instance | timeout. Two different errors.

%%% Created:  Fri May 21 16:16:19 2010 by R. Primus  <rprimus@gmail.com>
%%%


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
%%% Modified: ###mod_date### by ###mod_author###
%%% Note:     ###mod_note###
%%%
%%%
%%% Usage:
%%%
%%%
%%% ###USAGE###
%%%


-module(scid).
-vsn("$Id: scid.erl,v 1.1 2010/05/21 15:16:27 micro Exp $").
-author("R Primus").
-copyright("Copyright (c) � 2010, R Primus. All Rights Reserved").
-doc("").
-purpose("").


%% External exports
-export([
]).

%% Records
-record(s_header, {
  headerID = <<"SCID">>,
  headerSize = <<0:16>>,

  recordSize = <<0:16>>,
  version = <<0:8>>,
  unused1 = <<0:8>>,

  uTCStartIndex = <<0:16>>,

  reserve = <<0:288>>
}).

-record(s_intraday, {
  dateTime = <<0/float>>,

  open = <<0:32/float>>,
  high = <<0:32/float>>,
  low = <<0:32/float>>,
  close = <<0:32/float>>,

  numTrades = <<0:32>>,
  totalVolume = <<0:32>>,
  bidVolume = <<0:32>>,
  askVolume = <<0:32>>
}).



%% Macros

%% used for debugging
-define(L(Obj), io:format("LOG ~w ~p\n", [?LINE, Obj])).


%% Log messages are designed to instantiated lazily only if the logging level
%% permits a log message to be logged
-define(Log(LogFun,Level,Msg),
LogFun(?MODULE,?LINE,Level,fun()-> {Msg,[]} end)).
-define(Log2(LogFun,Level,Msg,Params),
LogFun(?MODULE,?LINE,Level,fun()-> {Msg,Params} end)).

%% Type definitions for typer.

-type(instTime() :: instance | timeout).

log(Module, Line, _Level, FormatFun) ->
{Format, Arguments} = FormatFun(),
io:format("~w:~b: "++ Format ++ "~n", [Module, Line] ++ Arguments).


%%----------------------------------------------
%% Exported Client Functions
%% Starting and Stopping the System
%%----------------------------------------------


%% vim: set et ts=2 sw=2 ai invlist si cul nu:
