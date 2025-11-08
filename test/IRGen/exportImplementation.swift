// RUN: %target-swift-frontend -emit-ir %s -O | %FileCheck %s --check-prefix=OPTIMIZED
// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s --check-prefix=NEGATIVE
// RUN: %target-swift-frontend -emit-ir -primary-file %s | %FileCheck %s --check-prefix=PRIMARY

// Make sure that @export(implementation) functions are lazily
// emitted in WMO builds.

// Both functions are eliminated in optimized builds; the first is inlined
// into the caller, and the second is eliminated since it was never called.

// OPTIMIZED-NOT: define weak_odr hidden swiftcc void @"$s20exportImplementation18referencedFunctionyyF"() {{.*}} {
// OPTIMIZED-NOT: define weak_odr hidden swiftcc void @"$s20exportImplementation20unreferencedFunctionyyF"() {{.*}} {

// The unreferenced function should be eliminated in an unoptimized WMO
// build too, since it was never referenced from inside the module.

// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$s20exportImplementation18referencedFunctionyyF"() {{.*}} {
// NEGATIVE-NOT: define linkonce_odr hidden swiftcc void @"$s20exportImplementation20unreferencedFunctionyyF"() {{.*}} {

// In non-WMO mode, both functions must be emitted since they could be
// referenced from other translation units.

// PRIMARY-LABEL: define weak_odr hidden swiftcc void @"$s20exportImplementation18referencedFunctionyyF"() {{.*}} {
// PRIMARY-LABEL: define weak_odr hidden swiftcc void @"$s20exportImplementation20unreferencedFunctionyyF"() {{.*}} {

@export(implementation) public func referencedFunction() {}
@export(implementation) public func unreferencedFunction() {}

public func referencesFunction() {
  referencedFunction()
}
