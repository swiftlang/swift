// RUN: %target-swift-frontend -emit-ir -module-name main -sanitize=address \
// RUN:   %s | %FileCheck %s


// CHECK: define {{.*}}@"$s4main8withAsanSiyF"() [[ASAN:#[0-9]+]]
public func withAsan() -> Int { 0 }

// CHECK: define {{.*}}@"$s4main6noAsanSiyF"() [[NO_ASAN:#[0-9]+]]
@instrumentation(disable: address)
public func noAsan() -> Int { 1 }

// Disabling a different kind of instrumentation doesn't affect ASan.
// CHECK: define {{.*}}@"$s4main7noTsan1SiyF"() [[ASAN]]
@instrumentation(disable: thread)
public func noTsan1() -> Int { 2 }

// 'default' re-enables instrumentation that was inherited.
// CHECK: define {{.*}}@"$s4main11defaultFuncSiyF"() [[ASAN]]
@instrumentation(default)
public func defaultFunc() -> Int { 3 }

@inline(never)
func callee(_ f: () -> Int) -> Int { f() }

@instrumentation(disable: address)
public func closures() -> Int {
  // CHECK: define {{.*}}@"$s4main8closuresSiyFSiyXEfU_"() [[NO_ASAN]]
  let a = callee { 1 }
  // CHECK: define {{.*}}@"$s4main8closuresSiyFSiyXEfU0_"() [[ASAN]]
  let b = callee { @instrumentation(default) in 2 }
  return a + b
}

// CHECK: attributes [[ASAN]] = {{{.*}}sanitize_address
// CHECK-NOT: attributes [[NO_ASAN]] = {{{.*}}sanitize_address
