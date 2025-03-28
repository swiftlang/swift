// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 %s

// REQUIRES: concurrency

actor SomeActor {}

@globalActor
struct GA1 {
  static let shared = SomeActor()
}

@globalActor
struct GA2 {
  static let shared = SomeActor()
}

// Global actor on local var/let.
do {
  @GA1 let stored: Int
  // expected-error@-1:12 {{local variable 'stored' cannot have a global actor}}{{none}}
  let _ = stored
  @GA1 var computed: Int {0}
  // expected-error@-1:12 {{local variable 'computed' cannot have a global actor}}{{none}}

  // FIXME: This diag is superfluous. The attr is not allowed anyway.
  // expected-error@+1 {{declaration can not have multiple global actor attributes ('GA2' and 'GA1')}}{{none}}
  @GA1 @GA2 var conflict: Int {0}
  // expected-error@-1 {{local variable 'conflict' cannot have a global actor}}{{none}}
}
