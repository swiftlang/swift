// RUN: %target-swift-frontend -typecheck -verify %s

func test1(asyncfp : () async -> Int, fp : () -> Int) /*async*/ {
  //_ = await asyncfp()
  //_ = await asyncfp() + asyncfp()
  _ = await asyncfp() + fp()
  _ = await fp() + 42  // expected-warning {{no calls to 'async' functions occur within 'await' expression}}
  _ = asyncfp() // expected-error {{call is 'async' but is not marked with 'await'}}
}

