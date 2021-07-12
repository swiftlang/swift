// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

@available(SwiftStdlib 5.5, *)
distributed struct StructNope {} // expected-error{{distributed' modifier cannot be applied to this declaration}}
@available(SwiftStdlib 5.5, *)
distributed class ClassNope {} // expected-error{{'distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
@available(SwiftStdlib 5.5, *)
distributed enum EnumNope {} // expected-error{{distributed' modifier cannot be applied to this declaration}}

@available(SwiftStdlib 5.5, *)
distributed actor DA {

  class func classFunc() {
    // expected-error@-1{{class methods are only allowed within classes; use 'static' to declare a static method}}
  }

  nonisolated distributed func nonisolatedDistributed() async {
    // expected-error@-1{{function 'nonisolatedDistributed()' cannot be both 'nonisolated' and 'distributed'}}{{3-15=}}
    fatalError()
  }

  distributed nonisolated func distributedNonisolated() async {
    // expected-error@-1{{function 'distributedNonisolated()' cannot be both 'nonisolated' and 'distributed'}}{{15-27=}}
    fatalError()
  }
}