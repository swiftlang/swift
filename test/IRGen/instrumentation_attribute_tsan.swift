// RUN: %target-swift-frontend -emit-ir -module-name main -sanitize=thread \
// RUN:   %s | %FileCheck %s

// REQUIRES: PTRSIZE=64

// CHECK: define {{.*}}@"$s4main8withTsanSiyF"() [[TSAN:#[0-9]+]]
public func withTsan() -> Int { 0 }

// CHECK: define {{.*}}@"$s4main6noTsanSiyF"() [[NO_TSAN:#[0-9]+]]
@instrumentation(disable: thread)
public func noTsan() -> Int { 1 }

// Disabling a different kind of instrumentation doesn't affect TSan.
// CHECK: define {{.*}}@"$s4main7noAsan1SiyF"() [[TSAN]]
@instrumentation(disable: address)
public func noAsan1() -> Int { 2 }

// CHECK: attributes [[TSAN]] = {{{.*}}sanitize_thread
// CHECK-NOT: attributes [[NO_TSAN]] = {{{.*}}sanitize_thread
