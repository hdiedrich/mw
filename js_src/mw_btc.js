var oracle_key = "92r2FtYSQcqQMgzoXs3AzDAtu7Q3hgXmRD2HpcDM7g7UgArcxq6";
var alice_key = "92pJFTW3srGK11RDeWkXqVv3H1MvWd2xeqkB8W2eWFaftsoRGNk";
var bob_key = "92SL8DDiEpTiaqWHtHufG8vW2wpZkwSrL3796oUDV6yaWLM3qnB";

var t2_sig_hash = "3E2BB5A134A4388910F4881D86ABA6A167370B8C6C3791E3002D50369881B12A";
var t2_sig_hash_flag ="SIGHASH_ALL";
var t2_sig_hash_flaganyonecanpay = "true";
var t2_raw = "010000000129BAA02D2A3082CA48DC9647F4368460C55C95547FE5714BE6764BA9F6FE5D530100000000FFFFFFFF01A086010000000000C9524104B0024CC9260F4200147598408624A7B55839F75249DA160A1906DEAEC006FDCB802B9A72D06F5E9EBC6CC108704E3789B8C515746022864E86457D96D8E116BF410457A6E187AF6DCAD28F678A92850610504AA64685B4D6F60CBC30C1A1407A0CE03DF1D51102EB09ACA7CA6DF77C06FE3EF6054E2EE9DAC7B5AC849F6E5C026B734104F37DF2954632C965C828DF1C09DCF70002861A981789D9DF20600987DCB3975A7DB3C2AD87B875D31103D7FFE43DED752CF1393A68053A5D0EC4061116AE553F53AE00000000";

function sign_t2_tx(priv_key, t2_sig_hash, t2_sig_hash_flag, t2-sig-hash-flaganyonecanpay, t2_raw) {
    t2 = decode_raw_tx(t2_raw);
    return t2_raw;
};


var decode_raw_tx;

decode_raw_tx = (function() {
  var parse_int, u16, u32, u64, u8, varchar, varint;
  parse_int = function(size) {
    return function(bytes) {
      var i, n, _i;
      n = 0;
      for (i = _i = 0; 0 <= size ? _i < size : _i > size; i = 0 <= size ? ++_i : --_i) {
        n += (bytes.shift() & 0xff) << (8 * i);
      }
      return n;
    };
  };
  u8 = function(bytes) {
    return bytes.shift();
  };
  u16 = parse_int(2);
  u32 = parse_int(4);
  u64 = function(bytes) {
    return bytes.splice(0, 8);
  };
  varint = function(bytes) {
    var n;
    switch (n = u8(bytes)) {
      case 0xfd:
        return u16(bytes);
      case 0xfe:
        return u32(bytes);
      case 0xff:
        return u64(bytes);
      default:
        return n;
    }
  };
  varchar = function(bytes) {
    return bytes.splice(0, varint(bytes));
  };
  return function(bytes) {
    var in_count, out_count, tx, ver, _i, _j;
    bytes = bytes.slice();
    ver = u32(bytes);
    if (ver !== 0x01) {
      throw new Error('Unsupported version');
    }
    tx = new Bitcoin.Transaction;
    in_count = varint(bytes);
    for (_i = 0; 0 <= in_count ? _i < in_count : _i > in_count; 0 <= in_count ? _i++ : _i--) {
      tx.addInput(new Bitcoin.TransactionIn({
        outpoint: {
          hash: bytesToHex((bytes.splice(0, 32)).reverse()),
          index: u32(bytes)
        },
        script: varchar(bytes),
        seq: u32(bytes)
      }));
    }
    out_count = varint(bytes);
    for (_j = 0; 0 <= out_count ? _j < out_count : _j > out_count; 0 <= out_count ? _j++ : _j--) {
      tx.addOutput(new Bitcoin.TransactionOut({
        value: u64(bytes),
        script: varchar(bytes)
      }));
    }
    tx.lock_time = u32(bytes);
    return tx;
  };
})();
