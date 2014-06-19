-define(API_ERROR(Error), begin {ErrorCode, ErrorMsg} = Error,
                                throw({api_error, {ErrorCode, ErrorMsg}}) end).
%% Error code 0 is catch-all == "Unknown Error"
-define(CONTRACT_ID_TYPE, {1, "contract id is not integer"}).

-define(PUBKEY_TYPE, {2, "pubkey is not base58check encoded binary"}).
-define(EC_PUBKEY_LEN, {3, "base58check ec pubkey is not correct length (51)"}).
-define(RSA_PUBKEY_LEN, {4, "hex enc rsa pubkey is not correct length (902)"}).

-define(SIGNATURE_TYPE, {5, "signature is not hex encoded binary"}).
-define(SIGNATURE_LEN, {6, "signature length is not 73, 72 or 71 bytes"}).
-define(EC_PUBKEY_MISMATCH, {12, "t2 signing pubkey is not giver nor taker"}).

-define(CONTRACT_FULL, {7, "contract full; has giver and taker"}).
-define(CONTRACT_NOT_FOUND, {8, "contract not found"}).

-define(CONTRACT_EMPTY, {9, "contract empty"}).
-define(CONTRACT_ONLY_GIVER, {10, "contract only has giver"}).
-define(CONTRACT_ONLY_TAKER, {11, "contract only has taker"}).
