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

// Global actor on global var/let accessors.

var globalObserved: Int = 0 {
  @GA1 willSet {}
  // expected-warning@-1:8 {{willSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-2:8 {{move global actor attribute to var 'globalObserved'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
  // expected-error@-3 {{must match the isolation}}
  @GA1 didSet {}
  // expected-warning@-1:8 {{didSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-2:8 {{move global actor attribute to var 'globalObserved'}}{{3-8=}}{{-5:1-1=@GA1 }}{{none}}
  // expected-error@-3 {{must match the isolation}}
}

var globalComputed: Int {
  @GA1 get {}
  // expected-warning@-1:8 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
  // expected-note@-2:8 {{move global actor attribute to var 'globalComputed'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
  // expected-error@-3 {{must match the isolation}}
  @GA1 set {}
  // expected-warning@-1:8 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-2:8 {{move global actor attribute to var 'globalComputed'}}{{3-8=}}{{-5:1-1=@GA1 }}{{none}}
  // expected-error@-3 {{must match the isolation}}
  @GA1 _modify {}
  // expected-warning@-1:8 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}
  // expected-note@-2:8 {{move global actor attribute to var 'globalComputed'}}{{3-8=}}{{-9:1-1=@GA1 }}{{none}}
  // expected-error@-3 {{must match the isolation}}
}

var globalComputedRead: Int {
  @GA1 _read {}
  // expected-warning@-1:8 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-2:8 {{move global actor attribute to var 'globalComputedRead'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
  // expected-error@-3 {{must match the isolation}}
}

// Global actor on local var/let accessors. No fix-it: global actor on local
// var/let is not allowed.
do {
  var observed: Int = 0 {
    @GA1 willSet {}
    // expected-warning@-1:10 {{willSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    // expected-error@-2 {{must match the isolation}}
    @GA1 didSet {}
    // expected-warning@-1:10 {{didSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    // expected-error@-2 {{must match the isolation}}
  }
  observed = 0
  let _ = observed

  var computed: Int {
    @GA1 get {}
    // expected-warning@-1:10 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
    // expected-error@-2 {{must match the isolation}}
    @GA1 set {}
    // expected-warning@-1:10 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    // expected-error@-2 {{must match the isolation}}
    @GA1 _modify {}
    // expected-warning@-1:10 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    // expected-error@-2 {{must match the isolation}}
  }

  var computedRead: Int {
    @GA1 _read {}
    // expected-warning@-1:10 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    // expected-error@-2 {{must match the isolation}}
  }

  var conflict: Int {
    get {}
    @GA1 @GA2 set {}
    // expected-error@-1 {{declaration can not have multiple global actor attributes ('GA2' and 'GA1')}}{{none}}
    // expected-warning@-2 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    // expected-error@-3 {{must match the isolation}}
  }
}

// Global actor on property/subscript accessors.
do {
  struct S {
    // No fix-it: storage explicitly isolated.
    nonisolated var computedNonisolated: Int {
      @GA1 set {}
      // expected-warning@-1 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-error@-2 {{must match the isolation}}
      get {}
    }

    // No fix-it: storage explicitly isolated.
    nonisolated subscript(subscriptNonisolated _: Int) -> Int {
      @GA1 set {}
      // expected-warning@-1 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-error@-2 {{must match the isolation}}
      get {}
    }

    var conflict: Int {
      get {}
      // FIXME: The note/fix-it isn't helpful here. We should probably not treat the decl as isolated to either when there is a conflict.
      // expected-note@+2 {{move global actor attribute to property 'conflict'}}{{12-17=}}{{-5:5-5=@GA2 }}{{none}}
      // expected-error@+1 {{declaration can not have multiple global actor attributes ('GA2' and 'GA1')}}{{none}}
      @GA1 @GA2 set {}
      // expected-warning@-1 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-error@-2 {{must match the isolation}}
    }
  }
}
