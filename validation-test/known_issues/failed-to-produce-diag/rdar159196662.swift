// rdar://159196662
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -DA -verify-additional-prefix a-
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -DB -verify-additional-prefix b-

#if A
protocol UIntPackable {
  static func fromUint(from uint: UInt) -> Self
}

struct Args<each T: UIntPackable> {
  static func unpack(from: [UInt], to: inout (repeat each T)) {
    var i = 0
    to = (repeat { () -> each T in // expected-a-error {{failed to produce diagnostic for expression; please submit a bug report}}
      let result = (each T).fromUint(from: from[i])
      i += 1
      return result
    }())
  }
}
#endif

#if B
func buildTupleFromArray<each U>(
  _ values: [Any],
  as type: (repeat each U).Type
) -> (repeat each U) {
  var extractIdx = 0
  return (repeat { // expected-b-error {{failed to produce diagnostic for expression; please submit a bug report}}
    let val = values[extractIdx]
    extractIdx += 1
    return val as! (each U)
  }())
}
#endif
