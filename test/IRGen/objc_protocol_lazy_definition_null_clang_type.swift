// RUN: %target-swift-frontend -emit-ir -swift-version 6 -parse-as-library %s | %FileCheck %s

// REQUIRES: objc_interop

// Regression test: compiling any source that triggers emission of
// lazy ObjC protocol metadata for `NSObjectProtocol` (in this case,
// holding a `DispatchSourceTimer?` forces that emission) previously
// crashed in `buildMethodDescriptor` /
// `buildPropertyAttributes` / `buildProperty` with
// `Assertion failed: (detail::isPresent(Val) && "dyn_cast on a non-existent value")`
// or `Assertion failed: (!isNull() && "Cannot retrieve a NULL type pointer")`.
//
// The root cause: `NSObjectProtocol`'s `description`/`debugDescription`
// properties are typed `Swift.String` in the SIL accessor lowering
// path taken here, and `IRGenModule::getClangType(Swift.String)` has
// no direct Clang mapping (the NSString bridging is handled elsewhere),
// so `getObjCPropertyType` returned a null `clang::CanQualType`. Three
// sites that consumed it dereferenced the null: two early-returns in
// `emitObjC{Getter,Setter}DescriptorParts` left
// `descriptor.impl` unset (nullptr), and
// `getObjCEncodingForPropertyType` forwarded a null type into Clang's
// `getObjCEncodingForType`.
//
// This test just needs to reach `emitLazyObjCProtocolDefinitions` for
// `NSObjectProtocol` without crashing.

import Dispatch

internal class MyClass {
    fileprivate var timer: DispatchSourceTimer?

    init() {}
}

// CHECK: define {{.*}}swiftcc {{.*}} @"$s{{.*}}7MyClassC{{.*}}timer{{.*}}vg"
