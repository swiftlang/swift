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
  func test(p: P) { // expected-warning {{protocol 'P' as a type must be explicitly marked as 'any'}}
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:16--1:17=<#generic parameter name#>}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: ((P))) { // expected-warning {{protocol 'P' as a type must be explicitly marked as 'any'}}
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:18--1:19=<#generic parameter name#>}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: any P) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:20--1:21=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: (inout any P)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{none}}
  }
}
do {
  func test(p: __shared (any P)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:30--1:31=<#generic parameter name#>}} {{-1:26--1:30=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: ((any P))) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:22--1:23=<#generic parameter name#>}} {{-1:18--1:22=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: any (P)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any (P)'; consider using a generic constraint instead}} {{-1:21--1:22=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: any   P) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:22--1:23=<#generic parameter name#>}} {{-1:16--1:22=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: (any  (P))) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any (P)'; consider using a generic constraint instead}} {{-1:23--1:24=<#generic parameter name#>}} {{-1:17--1:22=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: P.Type) { // expected-warning {{protocol 'P' as a type must be explicitly marked as 'any'}}
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}} {{-1:16--1:17=<#generic parameter name#>}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: (P).Type) { // expected-warning {{protocol 'P' as a type must be explicitly marked as 'any'}}
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any (P).Type'; consider using a generic constraint instead}} {{-1:17--1:18=<#generic parameter name#>}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: any P.Type) {
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}} {{-1:20--1:21=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  func test(p: any ((P).Type)) {
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any (P).Type'; consider using a generic constraint instead}} {{-1:22--1:23=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}

do {
  func test(p: P & Q) { // expected-warning {{protocol 'P' as a type must be explicitly marked as 'any'}}
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:16--1:21=<#generic parameter name#>}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: ((P & Q))) { // expected-warning {{protocol 'P' as a type must be explicitly marked as 'any'}}
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:18--1:23=<#generic parameter name#>}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: any P & Q) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:20--1:25=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: inout any P & Q) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{none}}
  }
}
do {
  func test(p: __shared (any P & Q)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:30--1:35=<#generic parameter name#>}} {{-1:26--1:30=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: ((any P & Q))) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:22--1:27=<#generic parameter name#>}} {{-1:18--1:22=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: any (P & Q)) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any (P & Q)'; consider using a generic constraint instead}} {{-1:21--1:26=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: any   P & Q) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:22--1:27=<#generic parameter name#>}} {{-1:16--1:22=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: (any  (P & Q))) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any (P & Q)'; consider using a generic constraint instead}} {{-1:23--1:28=<#generic parameter name#>}} {{-1:17--1:22=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: (P & Q).Type) { // expected-warning {{protocol 'P' as a type must be explicitly marked as 'any'}}
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any (P & Q).Type'; consider using a generic constraint instead}} {{-1:16--1:23=<#generic parameter name#>}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: ((P & Q)).Type) { // expected-warning {{protocol 'P' as a type must be explicitly marked as 'any'}}
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any ((P & Q)).Type'; consider using a generic constraint instead}} {{-1:18--1:23=<#generic parameter name#>}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: any (P & Q).Type) {
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any (P & Q).Type'; consider using a generic constraint instead}} {{-1:20--1:27=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}
do {
  func test(p: any (((P & Q)).Type)) {
    p.staticMethod(false) // expected-error {{member 'staticMethod' cannot be used on value of type 'any ((P & Q)).Type'; consider using a generic constraint instead}} {{-1:23--1:28=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
  }
}

do {
  // With an existing generic parameter list.
  func test<T: Sequence>(t: T, p: any P) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:39--1:40=<#generic parameter name#>}} {{-1:35--1:39=}} {{-1:24--1:24=, <#generic parameter name#>: P}} {{none}}
  }
}
do {
  // With an subscript expression.
  func test(p: any P) {
    p[false] // expected-error {{member 'subscript' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:20--1:21=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}
do {
  // With an initializer.
  func test(p: any P.Type) {
    p.init(false) // expected-error {{member 'init' cannot be used on value of type 'any P.Type'; consider using a generic constraint instead}} {{-1:20--1:21=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
  }
}

// Inside initializers, accessors and subscripts.
struct Test {
  init(p: any P) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:15--1:16=<#generic parameter name#>}} {{-1:11--1:15=}} {{-1:7--1:7=<<#generic parameter name#>: P>}} {{none}}
  }

  init<T: P>(p: any P, t: T) {
    p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:21--1:22=<#generic parameter name#>}} {{-1:17--1:21=}} {{-1:12--1:12=, <#generic parameter name#>: P}} {{none}}
  }

  subscript(p: any P) -> any P {
    p.method(false)  // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-1:20--1:21=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P>}} {{none}}
    return p
  }

  subscript<T: P>(p: any P, t: T) -> any P {
    get {
      p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-2:26--2:27=<#generic parameter name#>}} {{-2:22--2:26=}} {{-2:17--2:17=, <#generic parameter name#>: P}} {{none}}
      return p
    }
    set(value) {
      p.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{-6:26--6:27=<#generic parameter name#>}} {{-6:22--6:26=}} {{-6:17--6:17=, <#generic parameter name#>: P}} {{none}}
      value.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{none}}
    }
  }

  subscript(p: any P & Q) -> any P {
    _read { p.method(false) }   // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-1:20--1:25=<#generic parameter name#>}} {{-1:16--1:20=}} {{-1:12--1:12=<<#generic parameter name#>: P & Q>}} {{none}}
    _modify { p.method(false) } // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-2:20--2:25=<#generic parameter name#>}} {{-2:16--2:20=}} {{-2:12--2:12=<<#generic parameter name#>: P & Q>}} {{none}}
    willSet { p.method(false) } // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-3:20--3:25=<#generic parameter name#>}} {{-3:16--3:20=}} {{-3:12--3:12=<<#generic parameter name#>: P & Q>}} {{none}}
    didSet { p.method(false) }  // expected-error {{member 'method' cannot be used on value of type 'any P & Q'; consider using a generic constraint instead}} {{-4:20--4:25=<#generic parameter name#>}} {{-4:16--4:20=}} {{-4:12--4:12=<<#generic parameter name#>: P & Q>}} {{none}}
    // expected-error@-2 {{'willSet' is not allowed in subscripts}}
    // expected-error@-2 {{'didSet' is not allowed in subscripts}}
  }

  var property: any P {
    get {}
    set {
      newValue.method(false) // expected-error {{member 'method' cannot be used on value of type 'any P'; consider using a generic constraint instead}} {{none}}
    }
  }
}
