// RUN: %target-run-simple-swift(-I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Onone) | %FileCheck %s

// REQUIRES: executable_test

import RefCountedSmartPtrs

func conversions(_ r: RefCountedBase) {
    let ref = RefOfBase(r)
    let ptr = PtrOfBase(r)
    let _ = ref.asReference
    let _ = ptr.asReference!
    let derivedRef = DerivedRefOfBase(r)
    let _ = derivedRef.asReference
}

conversions(RefCountedBase())

// CHECK: created
// CHECK: destroyed
