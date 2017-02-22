class C1 : NSObject {
  func foo(a : Int, b: Int, c : Int) {}
  func foo1(a : Int, _: Int, c : Int) {}
  func foo2(_ : Int, _: Int, c : Int) {}
  func foo3() {}
  init(a : Int, b :Int) {}
  init(_ : Int) {}
}
func takeC1(a : Int, b: Int, c :Int) -> C1 {
  let C1Ins = C1(a: a, b: b)
  C1Ins.foo(a: a, b: b, c: c)
  C1Ins.foo1(a: a, b, c: c)
  C1Ins.foo2(a, b, c: c)
  C1Ins.foo3()
  let C1Ins2 = C1(a)
  return C1Ins2
}

@objc public enum CommonFix : Int {
  case A1
  case B1
  case C1
}

func takeCommonFix(_ f: CommonFix) -> Int {
  switch(f) {
  case .A1: break
  case .B1: break
  case .C1: break
    return 1
  }
  return 0
}

@objc public enum CustomError: Int, Error {
    case a1, b1
}

func takeError(_ e : CustomError) {
  switch(e) {
  case .a1: break
  case .b1: break
    return
  }
}

@objc(C2ObjC)
class C2 {}

@objc open class /*MyClass*/MyClass: NSObject {
  @objc open func /*MyClass_foo*/foo(/*MyClass_foo_p1*/bar: Int, /*MyClass_foo_p2*/baz: Int) {}
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=translate -swift-name "foo(a:b:c:)" -pos=11:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo(a1:b1:c1:)" -pos=11:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo(_:b1:c1:)" -pos=11:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK3 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo1(_:_:c2:)" -pos=11:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK4 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo1(_:_:_:)" -pos=11:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK5 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo2(a:b:c:)" -pos=12:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK6 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo2(_:_:_:)" -pos=12:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK7 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo1()" -pos=14:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK8 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo1(a:b:c:)" -pos=14:11 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK-NONE %s
// RUN: %sourcekitd-test -req=translate -swift-name "C11" -pos=1:8 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK9 %s

// RUN: %sourcekitd-test -req=translate -swift-name "init(a1:b2:)" -pos=10:16 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK10 %s
// RUN: %sourcekitd-test -req=translate -swift-name "init(_:_:)" -pos=10:16 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK11 %s
// RUN: %sourcekitd-test -req=translate -swift-name "foo(a1:_:)" -pos=10:16 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK12 %s

// RUN: %sourcekitd-test -req=translate -swift-name "A2" -pos=27:10 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK13 %s
// RUN: %sourcekitd-test -req=translate -swift-name "a2" -pos=27:10 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK13 %s
// RUN: %sourcekitd-test -req=translate -swift-name "a2" -pos=41:10 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK14 %s
// RUN: %sourcekitd-test -req=translate -swift-name "A2" -pos=41:10 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK14 %s
// RUN: %sourcekitd-test -req=translate -swift-name "C3" -pos=48:8 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK15 %s
// RUN: %sourcekitd-test -req=translate -swift-name "bar(_:other:)" -pos=51:36 %s -- -F %S/Inputs/mock-sdk -I %t.tmp %mcp_opt %s | %FileCheck -check-prefix=CHECK16 %s
dec
// CHECK-NONE: <empty name translation info>
// CHECK1: fooWithA:b:c:
// CHECK2: fooWithA1:b1:c1:
// CHECK3: foo:b1:c1:
// CHECK4: foo1::c2:
// CHECK5: foo1:::
// CHECK6: foo2WithA:b:c:
// CHECK7: foo2:::
// CHECK8: foo1
// CHECK9: C11

// CHECK10: initWithA1:b2:
// CHECK11: init::
// CHECK12: fooWithA1::
// CHECK13: CommonFixA2
// CHECK14: CustomErrorA2
// CHECK15: C2ObjC
// CHECK16: bar:other:
