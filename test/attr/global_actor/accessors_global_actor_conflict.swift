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

// Global actor on local var/let accessors. No fix-it: global actor on local
// var/let is not allowed.
do {
  var conflict: Int {
    get {}
    @GA1 @GA2 set {}
    // expected-error@-1 {{declaration can not have multiple global actor attributes ('GA2' and 'GA1')}}{{none}}
    // expected-warning@-2 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  }
}

// Global actor on property/subscript accessors.
do {
  struct S {
    var conflict: Int {
      get {}
      // FIXME: The note/fix-it isn't helpful here. We should probably not treat the decl as isolated to either when there is a conflict.
      // expected-note@+2 {{move global actor attribute to property 'conflict'}}{{12-17=}}{{-5:5-5=@GA2 }}{{none}}
      // expected-error@+1 {{declaration can not have multiple global actor attributes ('GA2' and 'GA1')}}{{none}}
      @GA1 @GA2 set {}
      // expected-warning@-1 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    }
  }
}
