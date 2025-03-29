// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

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


  // expected-error@+1{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  _ = intAsyncProp // expected-note{{property access is 'async'}}
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

  // expected-error@+1 {{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  _ = refTypeAsyncProp // expected-note {{property access is 'async'}}
}
