// RUN: %target-swift-frontend %s -typecheck -verify

func test() -> Any? {
  0 as! Any? // expected-warning {{forced cast from 'Int' to 'Any?' always succeeds; did you mean to use 'as'?}}
}
