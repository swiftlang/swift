// RUN: %sourcekitd-test -req=sema %s -- %s > %t.response
// RUN: %FileCheck -input-file=%t.response %s
// This tests that we are not crashing in SILGen.

// CHECK: @objc attribute used without importing module
@objc protocol Communicate {
  var name: String { get }
}
class Cat : Communicate {
  let name = "Felix"
}
