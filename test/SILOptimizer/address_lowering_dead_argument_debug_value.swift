// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -enable-sil-opaque-values -parse-as-library %s -o /dev/null

// An address-only argument whose only use is its debug_value. AddressLowering
// deletes the dead copy of the argument, which kills the debug_value's operand.
// The killed debug_value has to keep its address form, because the function has
// lowered addresses by then. Otherwise the verifier rejects an object-typed
// undef of an address-only type.

func borrowingArgument<T>(_ x: borrowing T) {}
