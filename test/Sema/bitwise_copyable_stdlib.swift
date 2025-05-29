// RUN: %target-typecheck-verify-swift                   \
// RUN:     -parse-stdlib                                \
// RUN:     -module-name Swift                           \
// RUN:     -disable-availability-checking               \
// RUN:     -debug-diagnostic-names

public protocol BitwiseCopyable {}

@frozen public struct S_Public_Frozen_Nonconforming_With_BuiltinInt64 {
  var i: Builtin.Int64
}

@available(*, unavailable)
extension S_Public_Frozen_Nonconforming_With_BuiltinInt64 : BitwiseCopyable {}

struct S_Nonconforming_With_2_SPFNWBI64 {
  var i1: S_Public_Frozen_Nonconforming_With_BuiltinInt64
  var i2: S_Public_Frozen_Nonconforming_With_BuiltinInt64
}
func nameTuple2S_Nonconforming_With_2_SPFNWBI64(_ t: (S_Nonconforming_With_2_SPFNWBI64, S_Nonconforming_With_2_SPFNWBI64)) {}

func take<T : BitwiseCopyable>(_ t: T) {}

protocol BearingBits {
  associatedtype Bits : BitwiseCopyable
}

func nameBearingBits<T : BearingBits>(_ t: T, _ b: T.Bits) {}
