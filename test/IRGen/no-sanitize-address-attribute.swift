// REQUIRES: asan_runtime
// REQUIRES: swift_feature_NoSanitize
// Verify @noSanitize(address) suppresses the LLVM sanitize_address
// function attribute (mirrors Clang's __attribute__((no_sanitize("address")))).

// RUN: %target-swift-frontend -emit-ir -sanitize=address \
// RUN:   -enable-experimental-feature NoSanitize %s | %FileCheck %s

// CHECK: define {{.*}}@"$s4main8withAsanSiyF"() [[ASAN:#[0-9]+]]
public func withAsan() -> Int { 0 }

// CHECK: define {{.*}}@"$s4main6noAsanSiyF"() [[NO_ASAN:#[0-9]+]]
@noSanitize(address)
public func noAsan() -> Int { 1 }

// A @noSanitize(thread) on an ASan build must NOT suppress ASan.
// CHECK: define {{.*}}@"$s4main7noTsan1SiyF"() [[ASAN]]
@noSanitize(thread)
public func noTsan1() -> Int { 2 }

// CHECK: attributes [[ASAN]] = { sanitize_address
// The @noSanitize(address) function has no enum attributes and so its
// attribute set opens with a string attribute like "frame-pointer".
// CHECK: attributes [[NO_ASAN]] = { "
