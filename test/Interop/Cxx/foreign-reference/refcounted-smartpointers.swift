// RUN: %target-run-simple-swift(-I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -Onone) | %FileCheck %s

// REQUIRES: executable_test

import RefCountedSmartPtrs

func conversions(_ r: RefCountedBase) {
    print("conversions")
    let ref = RefOfBase(r)
    let ptr = PtrOfBase(r)
    let _ = ref.asReference
    let _ = ptr.asReference!
    let derivedRef = DerivedRefOfBase(r)
    let _ = derivedRef.asReference
}

func bridgedFunctions(_ r: RefCountedBase) {
    print("bridging")
    bridgedFunction(r)
    bridgedFunction2(r)
    let x = bridgedFunction3()
    print(x.method())
    let y = bridgedFunction4()
    print(y!.method())
}

func takesOptional(_ r: RefCountedBase?) {}

func bridgedFunctionsWithOptionalInjection() {
    print("bridgingWithInjection")
    takesOptional(bridgedFunction3())
}

func notBridgedFunctions(_ r: RefCountedBase) {
    print("noBridging")
    var smartPtr = RefOfBase(r)
    notBridgedFunction(&smartPtr)
    notBridgedFunction2(RefOfBase(r))
    notBridgedFunction3(consuming: RefOfBase(r))
}

conversions(RefCountedBase())
// CHECK: created
// CHECK: conversions
// CHECK: destroyed
bridgedFunctions(RefCountedBase())
// CHECK: created
// CHECK: bridging
// CHECK: created
// CHECK: 42
// CHECK: created
// CHECK: 42
// CHECK: destroyed
// CHECK: destroyed
// CHECK: destroyed
bridgedFunctionsWithOptionalInjection()
// CHECK: bridgingWithInjection
// CHECK: created
// CHECK: destroyed
notBridgedFunctions(RefCountedBase())
// CHECK: created
// CHECK: noBridging
// CHECK: destroyed
