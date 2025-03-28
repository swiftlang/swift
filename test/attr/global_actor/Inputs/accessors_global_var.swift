actor SomeActor {}

@globalActor
struct GA1 {
  static let shared = SomeActor()
}

// Global actor on global var accessors.

// expected-swift6+-error@+4 {{not concurrency-safe}}
// expected-swift6+-note@+3 {{disable concurrency-safety}}
// expected-swift6+-note@+2 {{add '@MainActor'}}
// expected-swift6+-note@+1 {{convert 'globalObserved' to a 'let'}}
var globalObserved: Int = 0 {
  @GA1 willSet {}
  // expected-swift6+-error@-1:8 {{willSet observer cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{willSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-3:8 {{move global actor attribute to var 'globalObserved'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
  @GA1 didSet {}
  // expected-swift6+-error@-1:8 {{didSet observer cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{didSet observer cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-3:8 {{move global actor attribute to var 'globalObserved'}}{{3-8=}}{{-5:1-1=@GA1 }}{{none}}
}

var globalComputed: Int {
  @GA1 get {}
  // expected-swift7-error@-1:8 {{getter cannot have a global actor}}{{none}}
  // expected-swift6-warning@-2:8 {{getter cannot have a global actor; this will be an error in a future Swift language mode}}{{none}}
  // expected-swift5-warning@-3:8 {{getter cannot have a global actor; this will be an error in a future Swift language mode}}{{none}}
  // expected-note@-4:8 {{move global actor attribute to var 'globalComputed'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
  @GA1 set {}
  // expected-swift6+-error@-1:8 {{setter cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{setter cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-3:8 {{move global actor attribute to var 'globalComputed'}}{{3-8=}}{{-6:1-1=@GA1 }}{{none}}
  @GA1 _modify {}
  // expected-swift6+-error@-1:8 {{_modify accessor cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{_modify accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-3:8 {{move global actor attribute to var 'globalComputed'}}{{3-8=}}{{-10:1-1=@GA1 }}{{none}}
}

var globalComputedRead: Int {
  @GA1 _read {}
  // expected-swift6+-error@-1:8 {{_read accessor cannot have a global actor}}{{none}}
  // expected-swift5-warning@-2:8 {{_read accessor cannot have a global actor; this is an error in the Swift 6 language mode}}{{none}}
  // expected-note@-3:8 {{move global actor attribute to var 'globalComputedRead'}}{{3-8=}}{{-1:1-1=@GA1 }}{{none}}
}
