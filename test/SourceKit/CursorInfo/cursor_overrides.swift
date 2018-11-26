import FooClangModule

protocol Prot {
  func meth()
}

class Cls : S1, Prot {
  override func meth() {}
}

class SubCls : Cls {
  override func meth() {}
}

func goo(x: SubCls) {
  x.meth()
}

public protocol WithAssocType {
  /// Some kind of associated type
  associatedtype AssocType
}

public protocol WithInheritedAssocType : WithAssocType {
  associatedtype AssocType = Int
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=cursor -pos=16:7 %s -- -embed-bitcode -I %S/Inputs/cursor-overrides %mcp_opt %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: source.lang.swift.ref.function.method.instance (12:17-12:23)
// CHECK1: c:@M@cursor_overrides@objc(cs)SubCls(im)meth
// CHECK1: (SubCls) -> () -> ()
// CHECK1:      OVERRIDES BEGIN
// CHECK1-NEXT: c:@M@cursor_overrides@objc(cs)Cls(im)meth
// CHECK1-NEXT: s:16cursor_overrides4ProtP4methyyF
// CHECK1-NEXT: c:objc(cs)S1(im)meth
// CHECK1-NEXT: c:objc(cs)B1(im)meth
// CHECK1-NEXT: c:objc(pl)P1(im)meth
// CHECK1-NEXT: OVERRIDES END

// RUN: %sourcekitd-test -req=cursor -pos=25:20 %s -- -embed-bitcode -I %S/Inputs/cursor-overrides %mcp_opt %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: s:16cursor_overrides22WithInheritedAssocTypeP0eF0
// CHECK2: OVERRIDES BEGIN
// CHECK2-NEXT: s:16cursor_overrides13WithAssocTypeP0dE0
// CHECK2-NEXT: OVERRIDES END
