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
// CHECK:  publicAmbiguity()
// CHECK: note: found this candidate in module 'A'
// CHECK: public func publicAmbiguity() {}
// CHECK: note: found this candidate in module 'B'
// CHECK: public func publicAmbiguity() {}

  packageAmbiguity()
// CHECK: error: ambiguous use of 'packageAmbiguity()'
// CHECK-NEXT:  packageAmbiguity()
// CHECK: note: found this candidate in module 'A'
// CHECK: package func packageAmbiguity() {}
// CHECK: note: found this candidate in module 'B'
// CHECK: package func packageAmbiguity() {}

  internalAmbiguity()
// CHECK: error: ambiguous use of 'internalAmbiguity()'
// CHECK-NEXT:  internalAmbiguity()
// CHECK: note: found this candidate in module 'A'
// CHECK: internal func internalAmbiguity() {}
// CHECK: note: found this candidate in module 'B'
}
