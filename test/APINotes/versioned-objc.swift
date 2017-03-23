// RUN: rm -rf %t && mkdir -p %t

// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 4 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-4 %s
// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 3 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-3 %s

// REQUIRES: objc_interop

import APINotesFrameworkTest

// CHECK-DIAGS-4-NOT: versioned-objc.swift:[[@LINE-1]]:
class ProtoWithVersionedUnavailableMemberImpl: ProtoWithVersionedUnavailableMember {
  // CHECK-DIAGS-3: versioned-objc.swift:[[@LINE-1]]:7: error: type 'ProtoWithVersionedUnavailableMemberImpl' cannot conform to protocol 'ProtoWithVersionedUnavailableMember' because it has requirements that cannot be satisfied
  func requirement() -> Any? { return nil }
}

func testNonGeneric() {
  // CHECK-DIAGS-3:[[@LINE+1]]:{{[0-9]+}}: error: cannot convert value of type 'Any' to specified type 'Int'
  let _: Int = NewlyGenericSub.defaultElement()
  // CHECK-DIAGS-4:[[@LINE-1]]:{{[0-9]+}}: error: generic parameter 'Element' could not be inferred

  // CHECK-DIAGS-3:[[@LINE+1]]:{{[0-9]+}}: error: cannot specialize non-generic type 'NewlyGenericSub'
  let _: Int = NewlyGenericSub<Base>.defaultElement()
  // CHECK-DIAGS-4:[[@LINE-1]]:{{[0-9]+}}: error: cannot convert value of type 'Base' to specified type 'Int'
}

let unrelatedDiagnostic: Int = nil
