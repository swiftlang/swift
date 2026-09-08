// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// Reading an InlineArray element is not supported with opaque values, yet.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

typealias Tuple16 = (UInt8,UInt8,UInt8,UInt8,UInt8,UInt8,UInt8,UInt8,
                     UInt8,UInt8,UInt8,UInt8,UInt8,UInt8,UInt8,UInt8)

// The `bytes` value is a bitcast of a value which is loaded from a temporary
// stack location. LoadableByAddress must not let `bytes` alias that temporary,
// because it is destroyed before `bytes` is appended to the array.
@inline(never)
func copyOut(_ src: [Tuple16]) -> [InlineArray<16, UInt8>] {
    var out = [InlineArray<16, UInt8>]()
    for tuple in src {
        let bytes = withUnsafeBytes(of: tuple) {
            $0.withMemoryRebound(to: InlineArray<16, UInt8>.self) {
                $0[0]
            }
        }
        out.append(bytes)
    }
    return out
}

// CHECK: PASS
func testit() {
  let got = copyOut([(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)])[0]
  var ok = true
  for i in 0..<16 {
    if got[i] != 3 { ok = false }
  }
  print(ok ? "PASS" : "FAIL")
}

testit()
