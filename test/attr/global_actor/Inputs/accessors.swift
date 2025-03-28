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

// Global actor on top-level var accessors. No fix-it in Swift 6+: global actor
// not allowed on top-level var under complete strict concurrency.

var topLevelObserved: Int = 0 {
  @GA1 willSet {}
  // expected-swift6+-error@-1:8 {{willSet observer cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{willSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-swift5-note@-3:8 {{move global actor attribute to var 'topLevelObserved'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
  @GA1 didSet {}
  // expected-swift6+-error@-1:8 {{didSet observer cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{didSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-swift5-note@-3:8 {{move global actor attribute to var 'topLevelObserved'}}{{3-8=}}{{-5:1-1=@GA1 }}{{none}}
}

var topLevelComputed: Int {
  @GA1 get {}
  // expected-swift7-error@-1:8 {{getter cannot have a global actor}}{{none}}
  // expected-swift6-warning@-2:8 {{getter cannot have a global actor; this will be an error in a future Swift language mode}}{{none}}
  // expected-swift5-warning@-3:8 {{getter cannot have a global actor; this will be an error in a future Swift language mode}}{{none}}
  // expected-swift5-note@-4:8 {{move global actor attribute to var 'topLevelComputed'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
  @GA1 set {}
  // expected-swift6+-error@-1:8 {{setter cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-swift5-note@-3:8 {{move global actor attribute to var 'topLevelComputed'}}{{3-8=}}{{-6:1-1=@GA1 }}{{none}}
  @GA1 _modify {}
  // expected-swift6+-error@-1:8 {{_modify accessor cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-swift5-note@-3:8 {{move global actor attribute to var 'topLevelComputed'}}{{3-8=}}{{-10:1-1=@GA1 }}{{none}}
}

var topLevelComputedRead: Int {
  @GA1 _read {}
  // expected-swift6+-error@-1:8 {{_read accessor cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-swift5-note@-3:8 {{move global actor attribute to var 'topLevelComputedRead'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
}

// Global actor on local var accessors. No fix-it: global actor on local var is
// not allowed.
do {
  var observed: Int = 0 {
    @GA1 willSet {}
    // expected-swift6+-error@-1:10 {{willSet observer cannot have a global actor}}{{none}}
    // expected-swift5-warning@-2:10 {{willSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    @GA1 didSet {}
    // expected-swift6+-error@-1:10 {{didSet observer cannot have a global actor}}{{none}}
    // expected-swift5-warning@-2:10 {{didSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  }
  observed = 0
  let _ = observed

  var computed: Int {
    @GA1 get {}
    // expected-swift7-error@-1:10 {{getter cannot have a global actor}}{{none}}
    // expected-swift6-warning@-2:10 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
    // expected-swift5-warning@-3:10 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
    @GA1 set {}
    // expected-swift6+-error@-1:10 {{setter cannot have a global actor}}{{none}}
    // expected-swift5-warning@-2:10 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
    @GA1 _modify {}
    // expected-swift6+-error@-1:10 {{_modify accessor cannot have a global actor}}{{none}}
    // expected-swift5-warning@-2:10 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  }

  var computedRead: Int {
    @GA1 _read {}
    // expected-swift6+-error@-1:10 {{_read accessor cannot have a global actor}}{{none}}
    // expected-swift5-warning@-2:10 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  }
}

// Global actor on property/subscript accessors.
do {
  @GA1 struct S {
    var observed: Int {
      @GA1 willSet {}
      // expected-swift6+-error@-1:12 {{willSet observer cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{willSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to property 'observed'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 didSet {}
      // expected-swift6+-error@-1:12 {{didSet observer cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{didSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to property 'observed'}}{{7-12=}}{{-5:5-5=@GA1 }}{{none}}
    }

    var computed: Int {
      @GA1 get {}
      // expected-swift7-error@-1:12 {{getter cannot have a global actor}}{{none}}
      // expected-swift6-warning@-2:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-swift5-warning@-3:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-note@-4:12 {{move global actor attribute to property 'computed'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 set {}
      // expected-swift6+-error@-1:12 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to property 'computed'}}{{7-12=}}{{-6:5-5=@GA1 }}{{none}}
      @GA1 _modify {}
      // expected-swift6+-error@-1:12 {{_modify accessor cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to property 'computed'}}{{7-12=}}{{-10:5-5=@GA1 }}{{none}}
      @GA1 init {}
      // expected-swift6+-error@-1:12 {{init accessor cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{init accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to property 'computed'}}{{7-12=}}{{-14:5-5=@GA1 }}{{none}}
    }

    subscript(subscript _: Int) -> Int {
      @GA1 get {}
      // expected-swift7-error@-1:12 {{getter cannot have a global actor}}{{none}}
      // expected-swift6-warning@-2:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-swift5-warning@-3:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-note@-4:12 {{move global actor attribute to subscript 'subscript(subscript:)'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 set {}
      // expected-swift6+-error@-1:12 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to subscript 'subscript(subscript:)'}}{{7-12=}}{{-6:5-5=@GA1 }}{{none}}
      @GA1 _modify {}
      // expected-swift6+-error@-1:12 {{_modify accessor cannot have a global actor}}
      // expected-swift5-warning@-2:12 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}
      // expected-note@-3:12 {{move global actor attribute to subscript 'subscript(subscript:)'}}{{7-12=}}{{-10:5-5=@GA1 }}{{none}}
    }

    var computedRead: Int {
      @GA1 _read {}
      // expected-swift6+-error@-1:12 {{_read accessor cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to property 'computedRead'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
    }

    subscript(subscriptRead _: Int) -> Int {
      @GA1 _read {}
      // expected-swift6+-error@-1:12 {{_read accessor cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to subscript 'subscript(subscriptRead:)'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
    }

    // No fix-it: storage explicitly isolated.
    @GA1 var computedIsolated: Int {
      @GA2 set {}
      // expected-swift6+-error@-1:12 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      get {}
    }

    // No fix-it: storage explicitly isolated.
    @GA1 subscript(subscriptIsolated _: Int) -> Int {
      @GA1 set {}
      // expected-swift6+-error@-1:12 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      get {}
    }

    // No fix-it: storage explicitly isolated.
    nonisolated var computedNonisolated: Int {
      @GA1 set {}
      // expected-swift6+-error@-1:12 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      get {}
    }

    // No fix-it: storage explicitly isolated.
    nonisolated subscript(subscriptNonisolated _: Int) -> Int {
      @GA1 set {}
      // expected-swift6+-error@-1:12 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      get {}
    }
  }
}

// Global actor on protocol requirement accessors.
do {
  @GA1 protocol P {
    var property: Int {
      @GA1 get
      // expected-swift7-error@-1:12 {{getter cannot have a global actor}}{{none}}
      // expected-swift6-warning@-2:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-swift5-warning@-3:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-note@-4:12 {{move global actor attribute to property 'property'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 set
      // expected-swift6+-error@-1:12 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to property 'property'}}{{7-12=}}{{-6:5-5=@GA1 }}{{none}}
    }

    subscript(_: Int) -> Int {
      @GA1 get
      // expected-swift7-error@-1:12 {{getter cannot have a global actor}}{{none}}
      // expected-swift6-warning@-2:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-swift5-warning@-3:12 {{getter cannot have a global actor; this will be an error in a future Swift language}}{{none}}
      // expected-note@-4:12 {{move global actor attribute to subscript 'subscript(_:)'}}{{7-12=}}{{-1:5-5=@GA1 }}{{none}}
      @GA1 set
      // expected-swift6+-error@-1:12 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:12 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:12 {{move global actor attribute to subscript 'subscript(_:)'}}{{7-12=}}{{-6:5-5=@GA1 }}{{none}}
    }
  }
}

// Test fix-it with generic actor.
do {
  @GenericGlobalActor<Int> struct S {
    var property: Int {
      @GenericGlobalActor<Int> set {}
      // expected-swift6+-error@-1:32 {{setter cannot have a global actor}}{{none}}
      // expected-swift5-warning@-2:32 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
      // expected-note@-3:32 {{move global actor attribute to property 'property'}}{{7-32=}}{{-1:5-5=@GenericGlobalActor<Int> }}{{none}}
      get {}
    }
  }
}

