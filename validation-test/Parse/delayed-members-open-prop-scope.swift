// RUN: not %target-swift-frontend -typecheck -experimental-skip-all-function-bodies %s

var : Int

struct A {
  let prop: Int = {

struct B {
