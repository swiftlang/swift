// RUN: %target-swift-frontend %s -typecheck

struct S {
  let foo = "bar"
}
let s: S? = S()
let str: String? = "hello world"

switch str {
  case s?.foo?: ()
  default: ()
}
