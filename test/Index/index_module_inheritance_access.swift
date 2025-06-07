// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Mod.swiftmodule -module-name Mod %s
// RUN: %target-swift-ide-test -print-indexed-symbols -module-to-print Mod -source-filename %s -I %t | %FileCheck %s

public class C {
  fileprivate func foo() {}
}
public class D: C {
  public override func foo() {}
}

// Make sure we don't report the override of the private member in the base class.
//CHECK:      instance-method/Swift | foo() | s:3Mod1DC3fooyyF | Def,Dyn,RelChild | rel: 1
//CHECK-NEXT: RelChild | class/Swift | D | s:3Mod1DC
