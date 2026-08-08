// REQUIRES: tsan_runtime
// REQUIRES: PTRSIZE=64
// REQUIRES: swift_feature_NoSanitize
// Verify @noSanitize(thread) suppresses the LLVM sanitize_thread
// function attribute.

// RUN: %target-swift-frontend -emit-ir -sanitize=thread \
// RUN:   -enable-experimental-feature NoSanitize %s | %FileCheck %s

// CHECK: define {{.*}}@"$s4main8withTsanSiyF"() [[TSAN:#[0-9]+]]
public func withTsan() -> Int { 0 }

// CHECK: define {{.*}}@"$s4main6noTsanSiyF"() [[NO_TSAN:#[0-9]+]]
@noSanitize(thread)
public func noTsan() -> Int { 1 }

// A @noSanitize(address) on a TSan build must NOT suppress TSan.
// CHECK: define {{.*}}@"$s4main8noAsan1_SiyF"() [[TSAN]]
@noSanitize(address)
public func noAsan1_() -> Int { 2 }

// CHECK: attributes [[TSAN]] = { sanitize_thread
// CHECK: attributes [[NO_TSAN]] = { "
