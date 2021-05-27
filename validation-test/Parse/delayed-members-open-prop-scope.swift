// RUN: not %target-swift-frontend -typecheck -experimental-skip-all-function-bodies %s

struct A {
  let prop: Int = {

struct B {
