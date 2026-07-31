// REQUIRES: tsan_runtime
// REQUIRES: PTRSIZE=64
// Verify @_noSanitize(thread) suppresses the LLVM sanitize_thread
// function attribute.

// RUN: %target-swift-frontend -emit-ir -sanitize=thread %s | %FileCheck %s

// CHECK: define {{.*}}@"$s4main8withTsanSiyF"() [[TSAN:#[0-9]+]]
public func withTsan() -> Int { 0 }

// CHECK: define {{.*}}@"$s4main6noTsanSiyF"() [[NO_TSAN:#[0-9]+]]
@_noSanitize(thread)
public func noTsan() -> Int { 1 }

// A @_noSanitize(address) on a TSan build must NOT suppress TSan.
// CHECK: define {{.*}}@"$s4main8noAsan1_SiyF"() [[TSAN]]
@_noSanitize(address)
public func noAsan1_() -> Int { 2 }

// CHECK: attributes [[TSAN]] = { sanitize_thread
// CHECK: attributes [[NO_TSAN]] = { "
