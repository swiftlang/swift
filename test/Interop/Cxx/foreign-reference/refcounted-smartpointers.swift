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

func bridgedFunctionsWithRefAndPtrCtor(_ r: RefCountedBase) {
    print("withRefCtor")
    bridgedFunctionR(r)
    let returned = bridgedFunctionR_returns()
    print(returned.method())
}

// A smart pointer with both a T* and a const T* ctor. The non-const
// pointer ctor must be the one selected for implicit bridging.
func bridgedFunctionsWithConstPtrCtor(_ r: RefCountedBase) {
    print("withConstPtrCtor")
    bridgedFunctionConstPtr(r)
    let returned = bridgedFunctionConstPtr_returns()
    print(returned.method())
}

func bridgedFunctionsRefOnly(_ r: RefCountedBase) {
    print("refOnly")
    bridgedFunctionRefOnly(r)
    let returned = bridgedFunctionRefOnly_returns()
    print(returned.method())
}

// An unannotated derived smart pointer (no SWIFT_REFCOUNTED_PTR of its
// own) inheriting from the annotated Ref<T>. No implicit bridging but
// the inherited `asReference` property still works.
func unannotatedDerived() {
    print("unannotatedDerived")
    let smartPtr = makeUnannotatedRef()
    let frt = smartPtr.asReference
    print(frt.method())
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

bridgedFunctionsWithRefAndPtrCtor(RefCountedBase())
// CHECK: created
// CHECK: withRefCtor
// The pointer ctor must be the one selected for implicit bridging,
// even though a T& ctor is also available on RefR<T>.
// CHECK-NOT: RefR ref ctor
// CHECK: RefR ptr ctor
// CHECK-NOT: RefR ref ctor
// CHECK: RefR ptr ctor
// CHECK-NOT: RefR ref ctor
// CHECK: 42

bridgedFunctionsWithConstPtrCtor(RefCountedBase())
// CHECK: withConstPtrCtor
// The non-const T* ctor must be the one selected, never the const T* ctor.
// CHECK-NOT: RefConstPtr const-ptr ctor
// CHECK: RefConstPtr ptr ctor
// CHECK-NOT: RefConstPtr const-ptr ctor
// CHECK: RefConstPtr ptr ctor
// CHECK-NOT: RefConstPtr const-ptr ctor
// CHECK: 42

bridgedFunctionsRefOnly(RefCountedBase())
// CHECK: refOnly
// CHECK: RefRefOnly ref ctor
// CHECK: 42

unannotatedDerived()
// CHECK: unannotatedDerived
// CHECK: 42
