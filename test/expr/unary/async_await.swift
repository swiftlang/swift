// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency

func test1(asyncfp : () async -> Int, fp : () -> Int) async {
  _ = __await asyncfp()
  _ = __await asyncfp() + asyncfp()
  _ = __await asyncfp() + fp()
  _ = __await fp() + 42  // expected-warning {{no calls to 'async' functions occur within 'await' expression}}
  _ = asyncfp() // expected-error {{call is 'async' but is not marked with 'await'}}
}

