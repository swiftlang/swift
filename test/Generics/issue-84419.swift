// RUN: %target-typecheck-verify-swift -swift-version 6

struct G<T: Sequence> where T == Int {
// expected-error@-1 {{no type for 'T' can satisfy both 'T == Int' and 'T : Sequence'}}
// expected-error@-2 {{same-type requirement makes generic parameter 'T' non-generic}}
  let t: T
}
