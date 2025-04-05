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

@globalActor
struct GenericGlobalActor<T> {
  static var shared: SomeActor { SomeActor() }
}

// Global actor on property/subscript accessors.
do {
  @GA1 struct S {
    var observed: Int {
      @GA1 willSet {}
      // expected-warning@-1:12 {{willSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to property 'observed'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 didSet {}
      // expected-warning@-1:12 {{didSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to property 'observed'}}{{7-12=}}{{-4:5-5=@GA1 }}{{none}}
    }

    var computed: Int {
      @GA1 get {}
      // expected-warning@-1:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to property 'computed'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 set {}
      // expected-warning@-1:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to property 'computed'}}{{7-12=}}{{-4:5-5=@GA1 }}{{none}}
      @GA1 _modify {}
      // expected-warning@-1:12 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}
      // expected-note@-2:12 {{move global actor attribute to property 'computed'}}{{7-12=}}{{-7:5-5=@GA1 }}{{none}}
      @GA1 init {}
      // expected-warning@-1:12 {{init acecssor cannot have a global actor; this is an error in the Swift 6 language mode}}
      // expected-note@-2:12 {{move global actor attribute to property 'computed'}}{{7-12=}}{{-10:5-5=@GA1 }}{{none}}
    }

    subscript(subscript _: Int) -> Int {
      @GA1 get {}
      // expected-warning@-1 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-note@-2 {{move global actor attribute to subscript 'subscript(subscript:)'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 set {}
      // expected-warning@-1 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-2 {{move global actor attribute to subscript 'subscript(subscript:)'}}{{7-12=}}{{-4:5-5=@GA1 }}{{none}}
      @GA1 _modify {}
      // expected-warning@-1 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}
      // expected-note@-2 {{move global actor attribute to subscript 'subscript(subscript:)'}}{{7-12=}}{{-7:5-5=@GA1 }}{{none}}
    }

    var computedRead: Int {
      @GA1 _read {}
      // expected-warning@-1:12 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to property 'computedRead'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
    }

    subscript(subscriptRead _: Int) -> Int {
      @GA1 _read {}
      // expected-warning@-1 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-2 {{move global actor attribute to subscript 'subscript(subscriptRead:)'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
    }

    // No fix-it: storage explicitly isolated.
    @GA1 var computedIsolated: Int {
      @GA1 set {}
      // expected-warning@-1 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      get {}
    }

    // No fix-it: storage explicitly isolated.
    @GA1 subscript(subscriptIsolated _: Int) -> Int {
      @GA1 set {}
      // expected-warning@-1 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      get {}
    }
  }
}

// Global actor on protocol requirement accessors.
do {
  @GA1 protocol P {
    var property: Int {
      @GA1 get
      // expected-warning@-1:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to property 'property'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 set
      // expected-warning@-1:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to property 'property'}}{{7-12=}}{{-4:5-5=@GA1 }}{{none}}
    }

    subscript(_: Int) -> Int {
      @GA1 get
      // expected-warning@-1:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to subscript 'subscript(_:)'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 set
      // expected-warning@-1:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-2:12 {{move global actor attribute to subscript 'subscript(_:)'}}{{7-12=}}{{-4:5-5=@GA1 }}{{none}}
    }
  }
}

// Test fix-it with generic actor.
do {
  @GenericGlobalActor<Int> struct S {
    var property: Int {
      @GenericGlobalActor<Int> get {}
      // expected-warning@-1:32 {{getter cannot have a global actor; this will be an error in a future Swift language mode}}{{none}}
      // expected-note@-2:32 {{move global actor attribute to property 'property'}}{{7-32=}}{{-1:5-5=@GenericGlobalActor<Int> }}{{none}}
    }
  }
}

