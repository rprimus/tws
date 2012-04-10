%%% $Id: tws_app.erl,v 1.1 2010/05/16 18:10:19 micro Exp $
%%% Fri Mar  5 18:31:35 GMT 2010 -primus µ
%%% File:     tws_app.erl
%%% @author   R. Primus  <rprimus@gmail.com>
%%% @doc      ###description### 

%%% @reference <a href="http://oreilly.com/catalog/9780596518189/">Erlang Programming</a>,
%%%  <em> Francesco Cesarini and Simon Thompson</em>,
%%%  O'Reilly, 2009.
%%% @reference <a href="http://pragprog.com/titles/jaerlang/programming-erlang"> Programming Erlang</a>
%%%  <em> Joe Armstrong</em>,
%%%  Pragmatic Bookshelf, 2007.
%%% @copyright (c) ©  2010, R Primus. All Rights Reserved
%%%

%%% @headerfile ""

%%% Type definitions and comments for edoc.


%%% Created:  Sun May 16 19:08:34 2010 by R. Primus  <rprimus@gmail.com>
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


-module(tws_sup).
-vsn('1.0.0').
-author("R Primus").
-copyright("Copyright (c) © 2010, R Primus. All Rights Reserved").
-doc("").
-purpose("").

-behaviour(application).
-behaviour(supervisor).

%% External exports
-export([start/2, stop/1, init/1
]).

%% Records

%% Macros

%% used for debugging

start(_, _) ->
  case tws:start() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(State)  ->
  ok.

init(tws) ->
  ok.

%%----------------------------------------------
%% Exported Client Functions
%% Starting and Stopping the System
%%----------------------------------------------


%% vim: set et ts=2 sw=2 ai invlist si cul nu:
