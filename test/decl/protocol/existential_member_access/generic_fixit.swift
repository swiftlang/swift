// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A

  func method(_: A)
  subscript(_: A) -> A { get }
  init(_: A)

  static func staticMethod(_: A)
}
protocol Q {}

do {
  func test(p: P) { // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:16--1:17=some P}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: ((P))) { // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:18--1:19=some P}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: any P) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:20--1:21=some P}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: (inout any P)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: __shared (any P)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:30--1:31=some P}} {{-1:26--1:30=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: ((any P))) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:22--1:23=some P}} {{-1:18--1:22=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: any (P)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:21--1:22=some P}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: any   P) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:22--1:23=some P}} {{-1:16--1:22=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: (any  (P))) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:23--1:24=some P}} {{-1:17--1:22=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  func test(p: P.Type) { // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}} {{-1:16--1:17=(some P)}} {{none}}
    // expected-note@-1 {{member 'staticMethod' refrences 'Self.A', which cannot be resolved on type 'any P.Type'}}
  }
}
do {
  func test(p: (P).Type) { // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}} {{-1:17--1:18=some P}} {{none}}
    // expected-note@-1 {{member 'staticMethod' refrences 'Self.A', which cannot be resolved on type 'any P.Type'}}
  }
}
do {
  func test(p: any P.Type) {
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}} {{-1:20--1:21=(some P)}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'staticMethod' refrences 'Self.A', which cannot be resolved on type 'any P.Type'}}
  }
}
do {
  func test(p: any ((P).Type)) {
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}} {{-1:22--1:23=some P}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'staticMethod' refrences 'Self.A', which cannot be resolved on type 'any P.Type'}}
  }
}

do {
  func test(p: P & Q) { // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:16--1:21=some P & Q}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: ((P & Q))) { // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:18--1:23=some P & Q}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: any P & Q) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:20--1:25=some P & Q}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: inout any P & Q) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: __shared (any P & Q)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:30--1:35=some P & Q}} {{-1:26--1:30=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: ((any P & Q))) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:22--1:27=some P & Q}} {{-1:18--1:22=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: any (P & Q)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:21--1:26=some P & Q}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: any   P & Q) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:22--1:27=some P & Q}} {{-1:16--1:22=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: (any  (P & Q))) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:23--1:28=some P & Q}} {{-1:17--1:22=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }
}
do {
  func test(p: (P & Q).Type) { // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any (P & Q).Type'; consider using a generic constraint instead}} {{-1:17--1:22=some P & Q}} {{none}}
    // expected-note@-1 {{member 'staticMethod' refrences 'Self.A', which cannot be resolved on type 'any (P & Q).Type'}}
  }
}
do {
  func test(p: ((P & Q)).Type) { // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any (P & Q).Type'; consider using a generic constraint instead}} {{-1:18--1:23=some P & Q}} {{none}}
    // expected-note@-1 {{member 'staticMethod' refrences 'Self.A', which cannot be resolved on type 'any (P & Q).Type'}}
  }
}
do {
  func test(p: any (P & Q).Type) {
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any (P & Q).Type'; consider using a generic constraint instead}} {{-1:21--1:26=some P & Q}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'staticMethod' refrences 'Self.A', which cannot be resolved on type 'any (P & Q).Type'}}
  }
}
do {
  func test(p: any (((P & Q)).Type)) {
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any (P & Q).Type'; consider using a generic constraint instead}} {{-1:23--1:28=some P & Q}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'staticMethod' refrences 'Self.A', which cannot be resolved on type 'any (P & Q).Type'}}
  }
}

do {
  // With an existing generic parameter list.
  func test<T: Sequence>(t: T, p: any P) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:39--1:40=some P}} {{-1:35--1:39=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  // With an subscript expression.
  func test(p: any P) {
    p[false] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:20--1:21=some P}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'subscript' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }
}
do {
  // With an initializer.
  func test(p: any P.Type) {
    p.init(false) // expected-error {{member 'init' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}} {{-1:20--1:21=(some P)}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'init' refrences 'Self.A', which cannot be resolved on type 'any P.Type'}}
  }
}

// Inside initializers, accessors and subscripts.
struct Test {
  init(p: any P) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:15--1:16=some P}} {{-1:11--1:15=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }

  init<T: P>(p: any P, t: T) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:21--1:22=some P}} {{-1:17--1:21=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
  }

  subscript(p: any P) -> any P {
    p.method(false)  // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:20--1:21=some P}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
    return p
  }

  subscript<T: P>(p: any P, t: T) -> any P {
    get {
      p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-2:26--2:27=some P}} {{-2:22--2:26=}} {{none}}
      // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
      return p
    }
    set(value) {
      p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-7:26--7:27=some P}} {{-7:22--7:26=}} {{none}}
      // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
      value.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{none}}
      // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
    }
  }

  subscript(p: any P & Q) -> any P {
    _read { p.method(false) }   // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:20--1:25=some P & Q}} {{-1:16--1:20=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
    _modify { p.method(false) } // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-3:20--3:25=some P & Q}} {{-3:16--3:20=}} {{none}}
    // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
    willSet { p.method(false) } // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-5:20--5:25=some P & Q}} {{-5:16--5:20=}} {{none}}
    // expected-error@-1 {{'willSet' is not allowed in subscripts}}
    // expected-note@-2 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
    didSet { p.method(false) }  // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-8:20--8:25=some P & Q}} {{-8:16--8:20=}} {{none}}
    // expected-error@-1 {{'didSet' is not allowed in subscripts}}
    // expected-note@-2 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P & Q'}}
  }

  var property: any P {
    get {}
    set {
      newValue.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{none}}
      // expected-note@-1 {{member 'method' refrences 'Self.A', which cannot be resolved on type 'any P'}}
    }
  }
}
