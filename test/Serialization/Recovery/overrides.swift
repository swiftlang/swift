// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules | %FileCheck %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD -enable-experimental-deserialization-recovery | %FileCheck -check-prefix CHECK-RECOVERY %s

// REQUIRES: objc_interop

import Overrides

// Please use prefixes to keep the printed parts of this file in alphabetical
// order.

public class SwiftOnlyClass {}

public class A_Sub: Base {
  public override func disappearingMethod() {}
  public override func nullabilityChangeMethod() -> Any? { return nil }
  public override func typeChangeMethod() -> Any { return self }
  public override func disappearingMethodWithOverload() {}
}

// CHECK-LABEL: class A_Sub : Base {
// CHECK-NEXT: func disappearingMethod()
// CHECK-NEXT: func nullabilityChangeMethod() -> Any?
// CHECK-NEXT: func typeChangeMethod() -> Any
// CHECK-NEXT: func disappearingMethodWithOverload()
// CHECK-NEXT: init()
// CHECK-NEXT: {{^}$}}

// CHECK-RECOVERY-LABEL: class A_Sub : Base {
// CHECK-RECOVERY-NEXT: init()
// CHECK-RECOVERY-NEXT: {{^}$}}

extension Base {
  @nonobjc func disappearingMethodWithOverload() -> SwiftOnlyClass? { return nil }
}

public class B_GenericSub : GenericBase<Base> {
  public override func disappearingMethod() {}
  public override func nullabilityChangeMethod() -> Base? { return nil }
  public override func typeChangeMethod() -> Any { return self }
}

// CHECK-LABEL: class B_GenericSub : GenericBase<Base> {
// CHECK-NEXT: func disappearingMethod()
// CHECK-NEXT: func nullabilityChangeMethod() -> Base?
// CHECK-NEXT: func typeChangeMethod() -> Any
// CHECK-NEXT: init()
// CHECK-NEXT: {{^}$}}

// CHECK-RECOVERY-LABEL: class B_GenericSub : GenericBase<Base> {
// CHECK-RECOVERY-NEXT: init()
// CHECK-RECOVERY-NEXT: {{^}$}}


public class C1_IndexedSubscriptDisappears : IndexedSubscriptDisappearsBase {
  public override subscript(index: Int) -> Any { return self }
}

// CHECK-LABEL: class C1_IndexedSubscriptDisappears : IndexedSubscriptDisappearsBase {
// CHECK-NEXT: subscript(index: Int) -> Any { get }
// CHECK-NEXT: init()
// CHECK-NEXT: {{^}$}}

// CHECK-RECOVERY-LABEL: class C1_IndexedSubscriptDisappears : IndexedSubscriptDisappearsBase {
// CHECK-RECOVERY-NEXT: init()
// CHECK-RECOVERY-NEXT: {{^}$}}


public class C2_KeyedSubscriptDisappears : KeyedSubscriptDisappearsBase {
  public override subscript(key: Any) -> Any { return key }
}

// CHECK-LABEL: class C2_KeyedSubscriptDisappears : KeyedSubscriptDisappearsBase {
// CHECK-NEXT: subscript(key: Any) -> Any { get }
// CHECK-NEXT: init()
// CHECK-NEXT: {{^}$}}

// CHECK-RECOVERY-LABEL: class C2_KeyedSubscriptDisappears : KeyedSubscriptDisappearsBase {
// CHECK-RECOVERY-NEXT: init()
// CHECK-RECOVERY-NEXT: {{^}$}}


public class C3_GenericIndexedSubscriptDisappears : GenericIndexedSubscriptDisappearsBase<Base> {
  public override subscript(index: Int) -> Base { fatalError() }
}

// CHECK-LABEL: class C3_GenericIndexedSubscriptDisappears : GenericIndexedSubscriptDisappearsBase<Base> {
// CHECK-NEXT: subscript(index: Int) -> Base { get }
// CHECK-NEXT: init()
// CHECK-NEXT: {{^}$}}

// CHECK-RECOVERY-LABEL: class C3_GenericIndexedSubscriptDisappears : GenericIndexedSubscriptDisappearsBase<Base> {
// CHECK-RECOVERY-NEXT: init()
// CHECK-RECOVERY-NEXT: {{^}$}}


public class C4_GenericKeyedSubscriptDisappears : GenericKeyedSubscriptDisappearsBase<Base> {
  public override subscript(key: Any) -> Base { fatalError() }
}

// CHECK-LABEL: class C4_GenericKeyedSubscriptDisappears : GenericKeyedSubscriptDisappearsBase<Base> {
// CHECK-NEXT: subscript(key: Any) -> Base { get }
// CHECK-NEXT: init()
// CHECK-NEXT: {{^}$}}

// CHECK-RECOVERY-LABEL: class C4_GenericKeyedSubscriptDisappears : GenericKeyedSubscriptDisappearsBase<Base> {
// CHECK-RECOVERY-NEXT: init()
// CHECK-RECOVERY-NEXT: {{^}$}}
