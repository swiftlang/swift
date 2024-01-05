// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Compile the exact same source file into 2 different modules.
// RUN: %target-swift-frontend -emit-module -o %t/A.swiftmodule %t/Lib.swift \
// RUN:    -emit-module-source-info -module-name A -package-name pkg \
// RUN:    -enable-testing
// RUN: %target-swift-frontend -emit-module -o %t/B.swiftmodule %t/Lib.swift \
// RUN:    -emit-module-source-info -module-name B -package-name pkg \
// RUN:    -enable-testing

// Build a client importing both modules and hitting the ambiguities.
// RUN: not %target-swift-frontend -typecheck -I %t %t/Client.swift -package-name pkg 2> %t/out
// RUN: cat %t/out | %FileCheck %s

//--- Lib.swift
public func publicAmbiguity() {}
package func packageAmbiguity() {}
internal func internalAmbiguity() {}

//--- Client.swift
@testable import A
@testable import B

func foo() {
  publicAmbiguity()
// CHECK: error: ambiguous use of 'publicAmbiguity()'
// CHECK-NEXT:  publicAmbiguity()
// CHECK-NEXT:  ^
// CHECK-NEXT: Lib.swift:1:13: note: found this candidate in module 'A'
// CHECK-NEXT: public func publicAmbiguity() {}
// CHECK-NEXT:             ^
// CHECK-NEXT: Lib.swift:1:13: note: found this candidate in module 'B'
// CHECK-NEXT: public func publicAmbiguity() {}
// CHECK-NEXT:             ^

  packageAmbiguity()
// CHECK: error: ambiguous use of 'packageAmbiguity()'
// CHECK-NEXT:  packageAmbiguity()
// CHECK-NEXT:  ^
// CHECK-NEXT: Lib.swift:2:14: note: found this candidate in module 'A'
// CHECK-NEXT: package func packageAmbiguity() {}
// CHECK-NEXT:             ^
// CHECK-NEXT: Lib.swift:2:14: note: found this candidate in module 'B'
// CHECK-NEXT: package func packageAmbiguity() {}
// CHECK-NEXT:             ^

  internalAmbiguity()
// CHECK: error: ambiguous use of 'internalAmbiguity()'
// CHECK-NEXT:  internalAmbiguity()
// CHECK-NEXT:  ^
// CHECK-NEXT: Lib.swift:3:15: note: found this candidate in module 'A'
// CHECK-NEXT: internal func internalAmbiguity() {}
// CHECK-NEXT:             ^
// CHECK-NEXT: Lib.swift:3:15: note: found this candidate in module 'B'
// CHECK-NEXT: internal func internalAmbiguity() {}
// CHECK-NEXT:             ^
}
