// RUN: not %target-swift-frontend -typecheck %s

class A {
  var member: B

protocol B {
  typealias Elem = Undefined<Int, <#placeholder#>

typealias C = Undefined<Int
