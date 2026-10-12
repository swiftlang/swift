// RUN: %empty-directory(%t)

// RUN: %target-clang -x c -c %S/Inputs/refcount-shims.c -o %t/shim.o

// A retain that carries the strong refcount into deinitingBit must trap.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test -DOVERFLOW %s -c -o %t/overflow.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/overflow.o %t/shim.o -o %t/overflow.out -dead_strip
// RUN: %target-run not --crash %t/overflow.out

// Stopping one short of that value must not trap. The run above checks that
// nothing else in the test traps and makes it look like a pass.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test %s -c -o %t/ok.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/ok.o %t/shim.o -o %t/ok.out -dead_strip
// RUN: %target-run %t/ok.out | %FileCheck %s

// The same in deinit, where the count carries into deinitOverflowBit.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test -DDEINIT -DOVERFLOW %s -c -o %t/deinit-overflow.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/deinit-overflow.o %t/shim.o -o %t/deinit-overflow.out -dead_strip
// RUN: %target-run not --crash %t/deinit-overflow.out

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test -DDEINIT %s -c -o %t/deinit-ok.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/deinit-ok.o %t/shim.o -o %t/deinit-ok.out -dead_strip
// RUN: %target-run %t/deinit-ok.out | %FileCheck --check-prefix CHECK-DEINIT %s

// A release in deinit that borrows from deinitingBit must trap.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test -DDEINIT -DOVER_RELEASE %s -c -o %t/deinit-over-release.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/deinit-over-release.o %t/shim.o -o %t/deinit-over-release.out -dead_strip
// RUN: %target-run not --crash %t/deinit-over-release.out

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern
// REQUIRES: embedded_stdlib_default_codegen
// REQUIRES: PTRSIZE=64

// UNSUPPORTED: OS=wasip1
// UNSUPPORTED: OS=emscripten
// The RUN lines assert a trap via LLVM's `not --crash`, but emscripten-run.py
// passes its arguments to `node`, which then cannot load `not` as a JS module
// (`Error: Cannot find module 'not'`).

// Call swift_retain_n, swift_release_n, and swift_retainCount through C shims,
// as calling them directly upsets the compiler.
@_extern(c, "test_retain_n")
func test_retain_n(_ object: UnsafeMutableRawPointer, _ n: UInt32) -> UnsafeMutableRawPointer

@_extern(c, "test_release_n")
func test_release_n(_ object: UnsafeMutableRawPointer, _ n: UInt32)

@_extern(c, "test_retainCount")
func test_retainCount(_ object: UnsafeMutableRawPointer) -> Int

// retain_n and release_n take at most 256 at a time.
func retain(_ object: UnsafeMutableRawPointer, _ count: UInt32) {
  var remaining = count
  while remaining > 0 {
    let n = min(remaining, 256)
    _ = test_retain_n(object, n)
    remaining -= n
  }
}

func release(_ object: UnsafeMutableRawPointer, _ count: UInt32) {
  var remaining = count
  while remaining > 0 {
    let n = min(remaining, 256)
    test_release_n(object, n)
    remaining -= n
  }
}

final class Thing {
  var x: Int = 0

#if DEINIT
  deinit {
    let raw = unsafeBitCast(self, to: UnsafeMutableRawPointer.self)

#if OVER_RELEASE
    test_release_n(raw, 1)
    print("over-released")
#endif

    // In deinit, the count starts at 0 and the largest value that is not an
    // overflow is 0x3fff_ffff.
    print("deinit count \(test_retainCount(raw))")
    // CHECK-DEINIT: deinit count 0
    retain(raw, 0x3fff_ffff)
    print("deinit at max \(strongCount(raw) == 0xbfff_ffff) \(x) \(test_retainCount(raw) == 0x3fff_ffff)")
    // CHECK-DEINIT: deinit at max true 2 true

#if OVERFLOW
    _ = test_retain_n(raw, 1)
    print("retained past the maximum")
#endif

    release(raw, 0x3fff_ffff)
    print("deinit balanced")
    // CHECK-DEINIT: deinit balanced
  }
#endif
}

func strongCount(_ object: UnsafeMutableRawPointer) -> UInt64 {
  return unsafe object.load(fromByteOffset: MemoryLayout<UInt64>.size, as: UInt64.self) & 0xffff_ffff
}

@main
struct Main {
  static func main() {
#if DEINIT
    var t: Thing? = Thing()
    t!.x = 2
    t = nil
    print("deallocated")
    // CHECK-DEINIT: deallocated
#else
    let t = Thing()
    t.x = 1
    let raw = unsafeBitCast(t, to: UnsafeMutableRawPointer.self)

    // The count starts at 1, so this lands on 0x7fff_ffff: the largest value
    // that is not an overflow.
    retain(raw, 0x7fff_fffe)
    print("at max \(strongCount(raw) == 0x7fff_ffff) \(t.x) \(test_retainCount(raw) == 0x7fff_ffff)")
    // CHECK: at max true 1 true

#if OVERFLOW
    _ = test_retain_n(raw, 1)
    print("retained past the maximum")
#endif
#endif
  }
}
