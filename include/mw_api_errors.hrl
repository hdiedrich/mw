%% We use element/2 to avoid declaring variable in the macro
-define(API_ERROR(Error), throw({api_error, {element(1, Error), element(2, Error)}})).
%% Error code 0 is catch-all == "Unknown Error"
-define(CONTRACT_ID_TYPE, {1, "contract id is not integer"}).

-define(EC_PUBKEY_TYPE, {2, "ec pubkey is not base58check encoded"}).
-define(RSA_PUBKEY_TYPE, {22, "rsa pubkey is not pem encoded"}).
-define(EC_PUBKEY_LEN, {3, "base58check ec pubkey is not correct length (51)"}).
-define(RSA_PUBKEY_LEN, {4, "pem encoded rsa pubkey is not correct length (???)"}).

-define(SIGNATURE_TYPE, {5, "signature is not hex encoded"}).
-define(SIGNATURE_LEN, {6, "signature length is not 73, 72 or 71"}).
-define(EC_PUBKEY_MISMATCH, {7, "signing pubkey is not giver or taker"}).

-define(CONTRACT_FULL, {8, "contract full; has giver and taker"}).
-define(CONTRACT_NOT_FOUND, {9, "contract not found"}).

-define(CONTRACT_EMPTY, {10, "contract empty"}).
-define(CONTRACT_ONLY_GIVER, {11, "contract only has giver"}).
-define(CONTRACT_ONLY_TAKER, {12, "contract only has taker"}).

-define(CONTRACT_T2_NOT_COMPLETE, {13, "cannot get t3: t2 is not complete"}).

-define(T2_NOT_BROADCASTED,
        {14, "T2 was not broadcasted. Please verify keys/signatures."}).

-define(ADDRESS_TYPE, {15, "address is not base58check encoded"}).
-define(ADDRESS_LEN, {16, "address is not 27-34 chars long"}).

-define(NO_EVENT_OUTCOME, {17, "cannot get t3: no event outcome"}).

-define(T3_NOT_BROADCASTED,
        {18, "T3 was not broadcasted. Please verify signatures."}).

-define(CONTRACT_FINISHED, {19, "contract is finished"}).
