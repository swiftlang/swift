public struct Foo<T> {
  let prop: Int
}
extension Foo where T: AnyObject {
    public var foo: Int { return 1 }
}
_ = Foo<Int>(prop: 42).prop


// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=7:24 -length 4 %s -- %s | %FileCheck %s

// CHECK: source.lang.swift.ref.var.instance (2:7-2:11)
