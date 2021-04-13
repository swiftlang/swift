// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

var intAsyncProp : Int {
  get async { 0 }
}

var intThrowsProp : Int {
  get throws { 0 }
}

var asyncThrowsProp : Int {
  get async throws { try await intAsyncProp + intThrowsProp }
}

func hello() async {
  _ = intThrowsProp // expected-error{{property access can throw, but it is not marked with 'try' and the error is not handled}}

  _ = intAsyncProp // expected-error{{property access is 'async' but is not marked with 'await'}}
}

class C {
  var counter : Int = 0
}

var refTypeThrowsProp : C {
  get throws { return C() }
}

var refTypeAsyncProp : C {
  get async { return C() }
}

func salam() async {
  _ = refTypeThrowsProp // expected-error{{property access can throw, but it is not marked with 'try' and the error is not handled}}


  _ = refTypeAsyncProp // expected-error {{property access is 'async' but is not marked with 'await'}}
}
