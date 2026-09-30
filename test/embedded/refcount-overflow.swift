// RUN: %empty-directory(%t)

// RUN: %target-clang -x c -c %S/Inputs/refcount-shims.c -o %t/shim.o

// A retain that lands the strong refcount exactly on the reserved
// immortalRefCount value must trap.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test -DOVERFLOW %s -c -o %t/overflow.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/overflow.o %t/shim.o -o %t/overflow.out -dead_strip
// RUN: %target-run not --crash %t/overflow.out

// Stopping one short of that value must not trap. The run above checks that
// nothing else in the test traps and makes it look like a pass.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library -module-name test %s -c -o %t/ok.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/ok.o %t/shim.o -o %t/ok.out -dead_strip
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

// Call swift_retain_n through a C shim, as calling it directly upsets the
// compiler.
@_extern(c, "test_retain_n")
func test_retain_n(_ object: UnsafeMutableRawPointer, _ n: UInt32) -> UnsafeMutableRawPointer

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
    _ = test_retain_n(raw, 0x7fff_fffe)
    _ = test_retain_n(raw, 0x7fff_ffff)
    print("at max \(strongCount(raw) == 0xffff_fffe) \(t.x)")
    // CHECK: at max true 1

#if OVERFLOW
    _ = test_retain_n(raw, 1)
    print("retained past the maximum")
#endif
  }
}
