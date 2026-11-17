// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature OnceFunctions -no-allocations -c -o %t/a.o
// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a.o | %FileCheck %s --check-prefix=SYMBOLS
// RUN: %target-embedded-link %t/a.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --check-prefixes=CHECK,ONONE

// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature OnceFunctions -no-allocations -O -c -o %t/a_opt.o
// RUN: %llvm-nm --undefined-only --format=just-symbols %t/a_opt.o | %FileCheck %s --check-prefix=SYMBOLS
// RUN: %target-embedded-link %t/a_opt.o -o %t/a_opt.out
// RUN: %target-run %t/a_opt.out | %FileCheck %s --check-prefixes=CHECK,OPT

// With allocations, dropping an escaping closure also frees its context. The
// explicit optimization levels override the optimize test modes.
// RUN: %target-run-simple-swift(-Onone -enable-experimental-feature Embedded -enable-experimental-feature OnceFunctions -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop %target-embedded-posix-shim -D ALLOCATIONS) | %FileCheck %s --check-prefixes=CHECK,ALLOCATIONS,ONONE
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -enable-experimental-feature OnceFunctions -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop %target-embedded-posix-shim -D ALLOCATIONS) | %FileCheck %s --check-prefixes=CHECK,ALLOCATIONS,OPT

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_OnceFunctions
// REQUIRES: embedded_stdlib_default_codegen
// UNSUPPORTED: OS=linux-gnu && CPU=aarch64

// Dropping an at-most-once closure destroys its context without the runtime,
// which doesn't provide swift_release with -no-allocations.
// SYMBOLS-NOT: swift_release

struct Resource: ~Copyable {
  let id: Int
  deinit { print("deinit"); print(id) }
}

@inline(never)
func callIf(_ flag: Bool, _ body: @callable(once) () -> Int) -> Int {
  flag ? body() : 0
}

@inline(never)
func trivialCaptures(_ flag: Bool, _ x: Int) -> Int {
  callIf(flag) { x + 1 }
}

@inline(never)
func captureless(_ flag: Bool) -> Int {
  callIf(flag) { 42 }
}

@inline(never)
func consumedCapture(_ flag: Bool, _ id: Int) -> Int {
  let r = Resource(id: id)
  return callIf(flag) {
    let r = consume r
    return r.id
  }
}

@inline(never)
func borrowedCapture(_ flag: Bool, _ id: Int) -> Int {
  let r = Resource(id: id)
  let result = callIf(flag) { r.id }
  print("after call")
  return result
}

#if ALLOCATIONS
@inline(never)
func makeEscaping(_ id: Int) -> @callable(once) () -> Int {
  let r = Resource(id: id)
  return {
    let r = consume r
    return r.id
  }
}

@inline(never)
func escapingClosure(_ flag: Bool, _ id: Int) -> Int {
  let f = makeEscaping(id)
  if flag {
    return f()
  }
  print("dropping")
  return 0
}
#endif

@main
struct Main {
  static func main() {
    print(trivialCaptures(true, 1))
    // CHECK: 2
    print(trivialCaptures(false, 1))
    // CHECK-NEXT: 0
    print(captureless(true))
    // CHECK-NEXT: 42
    print(captureless(false))
    // CHECK-NEXT: 0

    print(consumedCapture(true, 10))
    // CHECK-NEXT: deinit
    // CHECK-NEXT: 10
    // CHECK-NEXT: 10
    print(consumedCapture(false, 20))
    // FIXME: The optimized programs leak the resource, because SILCombine
    // combines the closure's call in callIf without destroying the capture on
    // the path that drops the closure (rdar://192544750). Check for its deinit
    // with CHECK-NEXT once that's fixed.
    // ONONE-NEXT: deinit
    // ONONE-NEXT: 20
    // CHECK-NEXT: 0

    print(borrowedCapture(true, 30))
    // CHECK-NEXT: after call
    // CHECK-NEXT: deinit
    // CHECK-NEXT: 30
    // CHECK-NEXT: 30
    print(borrowedCapture(false, 40))
    // CHECK-NEXT: after call
    // CHECK-NEXT: deinit
    // CHECK-NEXT: 40
    // CHECK-NEXT: 0

#if ALLOCATIONS
    print(escapingClosure(true, 50))
    // ALLOCATIONS-NEXT: deinit
    // ALLOCATIONS-NEXT: 50
    // ALLOCATIONS-NEXT: 50
    print(escapingClosure(false, 60))
    // ALLOCATIONS-NEXT: dropping
    // ALLOCATIONS-NEXT: deinit
    // ALLOCATIONS-NEXT: 60
    // ALLOCATIONS-NEXT: 0
#endif
  }
}
