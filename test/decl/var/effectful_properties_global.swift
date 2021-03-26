// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

var intAsyncProp : Int {
  get async { 0 }
}

var intThrowsProp : Int { // expected-note 2 {{var declared here}}
  get throws { 0 }
}

var asyncThrowsProp : Int {
  // expected-warning@+1 {{reference to var 'intThrowsProp' is not concurrency-safe because it involves shared mutable state}}
  get async throws { try await intAsyncProp + intThrowsProp }
}

func hello() async {
  // expected-warning@+1 {{reference to var 'intThrowsProp' is not concurrency-safe because it involves shared mutable state}}
  _ = intThrowsProp // expected-error{{property access can throw, but it is not marked with 'try' and the error is not handled}}

  _ = intAsyncProp // expected-error{{property access is 'async' but is not marked with 'await'}}
}

class C {
  var counter : Int = 0
}

var refTypeThrowsProp : C { // expected-note {{var declared here}}
  get throws { return C() }
}

var refTypeAsyncProp : C {
  get async { return C() }
}

func salam() async {
  // expected-warning@+1 {{reference to var 'refTypeThrowsProp' is not concurrency-safe because it involves shared mutable state}}
  _ = refTypeThrowsProp // expected-error{{property access can throw, but it is not marked with 'try' and the error is not handled}}


  _ = refTypeAsyncProp // expected-error {{property access is 'async' but is not marked with 'await'}}
}