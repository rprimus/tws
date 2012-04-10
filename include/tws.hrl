-define(PORT, 7496).
-define(SERVER, "127.0.0.1").
-define(SERVER_VERSION, 47).
-define(CLIENT_VERSION, 46).

%/*outgoing message IDs */

-define(REQ_MKT_DATA, 1).
-define(CANCEL_MKT_DATA, 2).
-define(PLACE_ORDER, 3).
-define(CANCEL_ORDER, 4).
-define(REQ_OPEN_ORDERS, 5).
-define(REQ_ACCOUNT_DATA, 6).
-define(REQ_EXECUTIONS, 7).
-define(REQ_IDS, 8).
-define(REQ_CONTRACT_DATA, 9).
-define(REQ_MKT_DEPTH, 10).
-define(CANCEL_MKT_DEPTH, 11).
-define(REQ_NEWS_BULLETINS, 12).
-define(CANCEL_NEWS_BULLETINS, 13).
-define(SET_SERVER_LOGLEVEL, 14).
-define(REQ_AUTO_OPEN_ORDERS, 15).
-define(REQ_ALL_OPEN_ORDERS, 16).
-define(REQ_MANAGED_ACCTS, 17).
-define(REQ_FA, 18).
-define(REPLACE_FA, 19).
-define(REQ_HISTORICAL_DATA, 20).
-define(EXERCISE_OPTIONS, 21).
-define(REQ_SCANNER_SUBSCRIPTION, 22).
-define(CANCEL_SCANNER_SUBSCRIPTION, 23).
-define(REQ_SCANNER_PARAMETERS, 24).
-define(CANCEL_HISTORICAL_DATA, 25).
-define(REQ_CURRENT_TIME, 49).
-define(REQ_REAL_TIME_BARS, 50).
-define(CANCEL_REAL_TIME_BARS, 51).
-define(REQ_FUNDAMENTAL_DATA, 52).
-define(CANCEL_FUNDAMENTAL_DATA, 53).


% enum tws_incoming_ids {
-define(TICK_PRICE, 1).
-define(TICK_SIZE, 2).
-define(ORDER_STATUS, 3).
-define(ERR_MSG, 4).
-define(OPEN_ORDER, 5).
-define(ACCT_VALUE, 6).
-define(PORTFOLIO_VALUE, 7).
-define(ACCT_UPDATE_TIME, 8).
-define(NEXT_VALID_ID, 9).
-define(CONTRACT_DATA, 10).
-define(EXECUTION_DATA, 11).
-define(MARKET_DEPTH, 12).
-define(MARKET_DEPTH_L2, 13).
-define(NEWS_BULLETINS, 14).
-define(MANAGED_ACCTS, 15).
-define(RECEIVE_FA, 16).
-define(HISTORICAL_DATA, 17).
-define(BOND_CONTRACT_DATA, 18).
-define(SCANNER_PARAMETERS, 19).
-define(SCANNER_DATA, 20).
-define(TICK_OPTION_COMPUTATION, 21).
-define(TICK_GENERIC, 45).
-define(TICK_STRING, 46).
-define(TICK_EFP, 47).
-define(CURRENT_TIME, 49).
-define(REAL_TIME_BARS, 50).
-define(FUNDAMENTAL_DATA, 51).
-define(CONTRACT_DATA_END, 52).
-define(OPEN_ORDER_END, 53).
-define(ACCT_DOWNLOAD_END, 54).
-define(EXECUTION_DATA_END, 55).
-define(DELTA_NEUTRAL_VALIDATION, 56).
-define(TICK_SNAPSHOT_END, 57).


% enum TickType
-define(BID_SIZE, 0).
-define(BID, 1).
-define(ASK, 2).
-define(ASK_SIZE, 3).
-define(LAST, 4).
-define(LAST_SIZE, 5).
-define(HIGH, 6).
-define(LOW, 7).
-define(VOLUME, 8).
-define(CLOSE, 9).
-define(BID_OPTION_COMPUTATION, 10).
-define(ASK_OPTION_COMPUTATION, 11).
-define(LAST_OPTION_COMPUTATION, 12).
-define(MODEL_OPTION, 13).
-define(OPEN, 14).
-define(LOW_13_WEEK, 15).
-define(HIGH_13_WEEK, 16).
-define(LOW_16_WEEK, 17).
-define(HIGH_16_WEEK, 18).
-define(LOW_51_WEEK, 19).
-define(HIGH_52_WEEK, 20).
-define(AVG_VOLUME, 21).
-define(OPEN_INTEREST, 22).
-define(OPTION_HISTORICAL_VOL, 23).
-define(OPTION_IMPLIED_VOL, 24).
-define(OPTION_BID_EXCH, 25).
-define(OPTION_ASK_EXCH, 26).
-define(OPTION_CALL_OPEN_INTEREST, 27).
-define(OPTION_PUT_OPEN_INTEREST, 28).
-define(OPTION_CALL_VOLUME, 29).
-define(OPTION_PUT_VOLUME, 30).
-define(INDEX_FUTURE_PREMIUM, 31).
-define(BID_EXCH, 32).
-define(ASK_EXCH, 33).
-define(AUCTION_VOLUME, 34).
-define(AUCTION_PRICE, 35).
-define(AUCTION_IMBALANCE, 36).
-define(MARK_PRICE, 37).
-define(BID_EFP_COMPUTATION, 38).
-define(ASK_EFP_COMPUTATION, 39).
-define(LAST_EFP_COMPUTATION, 40).
-define(OPEN_EFP_COMPUTATION, 41).
-define(HIGH_EFP_COMPUTATION, 42).
-define(LOW_EFP_COMPUTATION, 43).
-define(CLOSE_EFP_COMPUTATION, 44).
-define(LAST_TIMESTAMP, 45).
-define(SHORTABLE, 46).
-define(FUNDAMENTAL_RATIOS, 47).
-define(RT_VOLUME, 48).
-define(HALTED, 49).
-define(NOT_SET, 50).


-define(TICK_TYPE, [
    {0, bid_size},
    {1, bid},
    {2, ask},
    {3, ask_size},
    {4, last},
    {5, last_size},
    {6, high},
    {7, low},
    {8, volume},
    {9, close},
    {10, bid_option_computation},
    {11, ask_option_computation},
    {12, last_option_computation},
    {13, model_option},
    {14, open},
    {15, low_13_week},
    {16, high_13_week},
    {17, low_16_week},
    {18, high_16_week},
    {19, low_51_week},
    {20, high_52_week},
    {21, avg_volume},
    {22, open_interest},
    {23, option_historical_vol},
    {24, option_implied_vol},
    {25, option_bid_exch},
    {26, option_ask_exch},
    {27, option_call_open_interest},
    {28, option_put_open_interest},
    {29, option_call_volume},
    {30, option_put_volume},
    {31, index_future_premium},
    {32, bid_exch},
    {33, ask_exch},
    {34, auction_volume},
    {35, auction_price},
    {36, auction_imbalance},
    {37, mark_price},
    {38, bid_efp_computation},
    {39, ask_efp_computation},
    {40, last_efp_computation},
    {41, open_efp_computation},
    {42, high_efp_computation},
    {43, low_efp_computation},
    {44, close_efp_computation},
    {45, last_timestamp},
    {46, shortable},
    {47, fundamental_ratios},
    {48, rt_volume},
    {49, halted},
    {50, not_set}]
).

-record(server, {
  ip=?SERVER,
  port=?PORT
}).

-record(contract,{
  symbol="CL",
  sectype="FUT",
  expiry="201105",
  strike="0.0000000",
  right="",
  multiplier="",
  exchange="NYMEX",
  pri_exch="",
  currency="USD",
  local_symbol="",
  tick_list="100,101,104,106,165,221,225"
}).

-record(contract_details,{
  summary=#contract{},
  marketName="",
  tradingClass="",
  conId="",
  minTick="",
  multiplier="",
  orderTypes="",
  validExchanges="",
  priceMagnifier="",
  underConId="",
  longName="",
  primaryExchange="",
  contractMonth="",
  industry="",
  category="",
  subcategory="",
  timeZoneId="",
  tradingHours="",
  liquidHours=""
}).

% vim: set et ts=2 sw=2 ai invlist si cul nu:
