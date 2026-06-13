// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// `@inline(never)` keeps each conversion as an actual runtime call rather
// than letting the optimizer fold the literal.
@inline(never)
func f32(_: Void = ()) -> Float { 0 }
@inline(never)
func f64(_: Void = ()) -> Double { 0 }

@inline(never)
func check(_ actual: Float, _ expected: Float, _ label: StaticString) {
  if actual == expected {
    print("ok ", terminator: "")
    print(label)
  } else {
    print("FAIL ", terminator: "")
    print(label)
  }
}

@inline(never)
func check(_ actual: Double, _ expected: Double, _ label: StaticString) {
  if actual == expected {
    print("ok ", terminator: "")
    print(label)
  } else {
    print("FAIL ", terminator: "")
    print(label)
  }
}

@main
struct Main {
  static func main() {
    // Single-chunk positive literals
    let a32: Float = 0
    check(a32, f32(), "Float 0")
    let b32: Float = 1
    check(b32, f32() + 1, "Float 1")
    let c32: Float = 100
    check(c32, f32() + 100, "Float 100")

    // Single-chunk negative literals
    let d32: Float = -1
    check(d32, f32() - 1, "Float -1")
    let e32: Float = -100
    check(e32, f32() - 100, "Float -100")

    // Same set on Double
    let a64: Double = 0
    check(a64, f64(), "Double 0")
    let b64: Double = 1
    check(b64, f64() + 1, "Double 1")
    let c64: Double = 100
    check(c64, f64() + 100, "Double 100")
    let d64: Double = -1
    check(d64, f64() - 1, "Double -1")
    let e64: Double = -100
    check(e64, f64() - 100, "Double -100")

    // Multi-chunk: a positive value that needs more than one 64-bit chunk
    // to represent in two's complement (i.e., `bitWidth > 64`). 2^65 has
    // bitWidth 67 (a leading sign bit plus 66 magnitude bits).
    //
    // 2^65 == 0x1p65 in both Float and Double precision (exact).
    let multi32: Float = 36893488147419103232  // 2^65
    check(multi32, 0x1p65 as Float, "Float 2^65")
    let multi64: Double = 36893488147419103232  // 2^65
    check(multi64, 0x1p65 as Double, "Double 2^65")

    // Multi-chunk negative value: -(2^65).
    let negMulti32: Float = -36893488147419103232
    check(negMulti32, -0x1p65 as Float, "Float -2^65")
    let negMulti64: Double = -36893488147419103232
    check(negMulti64, -0x1p65 as Double, "Double -2^65")

    print("done")
    // CHECK: ok Float 0
    // CHECK-NEXT: ok Float 1
    // CHECK-NEXT: ok Float 100
    // CHECK-NEXT: ok Float -1
    // CHECK-NEXT: ok Float -100
    // CHECK-NEXT: ok Double 0
    // CHECK-NEXT: ok Double 1
    // CHECK-NEXT: ok Double 100
    // CHECK-NEXT: ok Double -1
    // CHECK-NEXT: ok Double -100
    // CHECK-NEXT: ok Float 2^65
    // CHECK-NEXT: ok Double 2^65
    // CHECK-NEXT: ok Float -2^65
    // CHECK-NEXT: ok Double -2^65
    // CHECK-NEXT: done
  }
}
