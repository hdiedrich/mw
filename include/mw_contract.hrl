%% Happy path states
-define(CONTRACT_STATE_DESC_CREATED, "Contract created.").

-define(CONTRACT_STATE_DESC_GIVER_ENTERED, "Giver entered contract.").
-define(CONTRACT_STATE_DESC_TAKER_ENTERED, "Taker entered contract.").

-define(CONTRACT_STATE_DESC_GIVER_T1, "Giver sent T1.").
-define(CONTRACT_STATE_DESC_TAKER_T1, "Taker sent T1.").

-define(CONTRACT_STATE_DESC_GIVER_SIGNED_T2, "Giver signed T2.").
-define(CONTRACT_STATE_DESC_TAKER_SIGNED_T2, "Taker signed T2.").
-define(CONTRACT_STATE_DESC_T2_BROADCASTED,  "T2 broadcasted.").
-define(CONTRACT_STATE_DESC_T2_MINED,        "T2 mined.").

-define(CONTRACT_STATE_DESC_EVENT_OUTCOME_HAPPENED, "Event outcome happened.").

-define(CONTRACT_STATE_DESC_GIVER_SIGNED_T3, "Giver signed T3.").
-define(CONTRACT_STATE_DESC_TAKER_SIGNED_T3, "Taker signed T3.").
-define(CONTRACT_STATE_DESC_ORACLE_SIGNED_T3, "Oracle signed T3.").

-define(CONTRACT_STATE_DESC_T3_BROADCASTED, "T3 broadcasted.").
-define(CONTRACT_STATE_DESC_T3_MINED, "T3 mined.").

%% Abort events
-define(CONTRACT_STATE_DESC_GIVER_ABORT_BEFORE_T1, "Giver aborted before hir T1.").
-define(CONTRACT_STATE_DESC_TAKER_ABORT_BEFORE_T1, "Taker aborted before hir T1.").

%% This is always after T1 was done but before T2 is broadcasted; since after
%% T2 is broadcasted and acknowledged by network, it cannot be aborted, save for
%% a blockchain re-organization & successful doubles-spend of the T1 output.
-define(CONTRACT_STATE_DESC_GIVER_ABORT_AFTER_T1, "Giver aborted after hir T1.").
-define(CONTRACT_STATE_DESC_TAKER_ABORT_AFTER_T1, "Taker aborted after hir T1.").
