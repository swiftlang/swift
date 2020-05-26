// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

class Outer {
  class Foo {
    var zim = Bar()
    var bas = Outer()
  }
  class Boo {
    var bas = Outer()
    var zim = Bar()
  }

  required init() {}
}

protocol Initable { init() }
extension Outer: Initable {}

class GFoo<T: Initable> {
  var zim = Bar()
  var bas = T()
}
class GBoo<T: Initable> {
  var bas = T()
  var zim = Bar()
}
class GFos<T: Initable> {
  var bar = T()
  var zim = Bar()
  var bas = T()
}

struct Bar { }

do {
  let a = Outer.Foo()
  let b = Outer.Boo()
  let c = GFoo<Outer>()
  let d = GBoo<Outer>()
  let e = GFos<Outer>()
}

// CHECK: Job's finished
print("Job's finished")
