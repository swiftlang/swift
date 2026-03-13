// RUN: %target-typecheck-verify-swift

enum E: Error { case e }

// rdar://106598067 – Make sure we don't crash.
let fn = {
  do {} catch let x as? E {}
  // expected-error@-1 {{cannot conditionally downcast in a type-casting pattern}}{{23-24=}}
  // expected-error@-2 {{expression pattern of type 'E?' cannot match values of type 'any Error'}}
  // expected-warning@-3 {{'catch' block is unreachable because no errors are thrown in 'do' block}}
}

// https://github.com/swiftlang/swift/issues/44631
let maybeInt: Any = 1
switch maybeInt {
case let intValue as? Int: _ = intValue
  // expected-error@-1 {{cannot conditionally downcast in a type-casting pattern}}{{21-22=}}
  // expected-error@-2 {{expression pattern of type 'Int?' cannot match values of type 'Any'}}
case let intValue as! Int: _ = intValue
  // expected-error@-1 {{cannot force downcast in a type-casting pattern}}{{21-22=}}
case let intValue is Int: _ = intValue
  // expected-error@-1 {{use 'as' keyword to bind a matched value}}{{19-21=as}}
default: break
}
