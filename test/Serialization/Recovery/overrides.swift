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