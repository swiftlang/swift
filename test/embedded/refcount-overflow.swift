// RUN: %empty-directory(%t)

// A retain that lands the strong refcount exactly on the reserved
// immortalRefCount value must trap.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test -DOVERFLOW %s -c -o %t/overflow.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/overflow.o -o %t/overflow.out -dead_strip
// RUN: %target-run not --crash %t/overflow.out

// Stopping one short of that value must not trap, so the test above cannot pass
// by trapping for some other reason.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test %s -c -o %t/ok.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/ok.o -o %t/ok.out -dead_strip
// RUN: %target-run %t/ok.out | %FileCheck %s

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

// On 64-bit the strong refcount is the low 32 bits of a word it shares with the
// weak refcount and the doNotFree bit, so an overflow would corrupt them.
// swift_retain_n is called directly because 2^32 ordinary retains is not a test.

@_extern(c, "swift_retain_n")
func swift_retain_n(_ object: UnsafeMutableRawPointer, _ n: UInt32) -> UnsafeMutableRawPointer

final class Thing {
  var x: Int = 0
}

func strongCount(_ object: UnsafeMutableRawPointer) -> UInt64 {
  return unsafe object.load(fromByteOffset: MemoryLayout<UInt64>.size, as: UInt64.self) & 0xffff_ffff
}

@main
struct Main {
  static func main() {
    let t = Thing()
    t.x = 1
    let raw = unsafeBitCast(t, to: UnsafeMutableRawPointer.self)

    // The count starts at 1, and immortalRefCount is 0xffff_ffff, so these two
    // land on 0xffff_fffe: the largest value that is not an overflow.
    _ = swift_retain_n(raw, 0x7fff_fffe)
    _ = swift_retain_n(raw, 0x7fff_ffff)
    print("at max \(strongCount(raw) == 0xffff_fffe) \(t.x)")
    // CHECK: at max true 1

#if OVERFLOW
    _ = swift_retain_n(raw, 1)
    print("retained past the maximum")
#endif
  }
}
