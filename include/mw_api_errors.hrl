%% Error code 0 is catch-all == "Unknown Error"
-define(API_ERROR_CONTRACT_ID_TYPE, {1, "contract id is not integer"}).
-define(API_ERROR_PUBKEY_TYPE, {2, "pubkey is not hex encoded"}).
-define(API_ERROR_EC_PUBKEY_LEN, {3, "hex enc ec pubkey is not correct length (130)"}).
-define(API_ERROR_RSA_PUBKEY_LEN, {4, "hex enc rsa pubkey is not correct length (902)"}).
