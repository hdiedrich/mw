-define(API_ERROR(Error), begin {ErrorCode, ErrorMsg} = Error,
                                throw({api_error, {ErrorCode, ErrorMsg}}) end).
%% Error code 0 is catch-all == "Unknown Error"
-define(CONTRACT_ID_TYPE, {1, "contract id is not integer"}).
-define(PUBKEY_TYPE, {2, "pubkey is not hex encoded"}).
-define(EC_PUBKEY_LEN, {3, "hex enc ec pubkey is not correct length (130)"}).
-define(RSA_PUBKEY_LEN, {4, "hex enc rsa pubkey is not correct length (902)"}).
-define(CONTRACT_FULL, {5, "contract full; has giver and taker"}).
-define(CONTRACT_NOT_FOUND, {5, "contract not found"}).
