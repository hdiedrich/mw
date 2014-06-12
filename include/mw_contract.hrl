%% Happy path states
-define(STATE_DESC_CREATED, <<"Contract created.">>).
-define(STATE_DESC_CLONED,  <<"Contract cloned.">>).

-define(STATE_DESC_GIVER_ENTERED, <<"Giver entered contract.">>).
-define(STATE_DESC_TAKER_ENTERED, <<"Taker entered contract.">>).

-define(STATE_DESC_GIVER_T1, <<"Giver sent T1.">>).
-define(STATE_DESC_TAKER_T1, <<"Taker sent T1.">>).

-define(STATE_DESC_GIVER_SIGNED_T2, <<"Giver signed T2.">>).
-define(STATE_DESC_TAKER_SIGNED_T2, <<"Taker signed T2.">>).

-define(STATE_DESC_T2_BROADCASTED,  <<"T2 broadcasted.">>).

-define(STATE_DESC_T2_MINED,        <<"T2 mined.">>).

-define(STATE_DESC_EVENT_OUTCOME_HAPPENED, <<"Event outcome happened.">>).

%% TODO: clarify if we need these
-define(STATE_DESC_GIVER_SIGNED_T3, <<"Giver signed T3.">>).
-define(STATE_DESC_TAKER_SIGNED_T3, <<"Taker signed T3.">>).
-define(STATE_DESC_ORACLE_SIGNED_T3, <<"Oracle signed T3.">>).

%% Use this for MVP
-define(STATE_DESC_SIGNED_T3, <<"T3 signed.">>).

-define(STATE_DESC_T3_BROADCASTED, <<"T3 broadcasted.">>).
-define(STATE_DESC_T3_MINED, <<"T3 mined.">>).

%% Abort events
-define(STATE_DESC_GIVER_ABORT_BEFORE_T1, <<"Giver aborted before her T1.">>).
-define(STATE_DESC_TAKER_ABORT_BEFORE_T1, <<"Taker aborted before her T1.">>).

%% This is always after T1 was done but before T2 is broadcasted; since after
%% T2 is broadcasted and acknowledged by network, it cannot be aborted, save for
%% a blockchain re-organization & successful doubles-spend of the T1 output.
-define(STATE_DESC_GIVER_ABORT_AFTER_T1, <<"Giver aborted after her T1.">>).
-define(STATE_DESC_TAKER_ABORT_AFTER_T1, <<"Taker aborted after her T1.">>).
