// rdar://173716481
// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple

func swapFirst(data: inout [UInt8]) {
  var span = data.mutableSpan
  swap(span[i], span[i+1]) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  // expected-error@-1 2 {{cannot find 'i' in scope}}
}
