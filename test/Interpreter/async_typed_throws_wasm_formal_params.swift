// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library -Onone %s -o %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=wasip1

// Regression test for the wasm async typed-throws t2t wrapper's formal-param
// arity bug (swiftlang/swift#89320 follow-up): the wrapper's argument-walking
// loop iterates over `outType->getParameters().size()` and consumes one LLVM
// arg per iteration, but a SIL formal parameter can expand to multiple LLVM
// args (e.g., String = 3 wasm32 words, multi-word structs, tuples).
//
// On wasm32, `(Int, String, Bool) async throws(LargeErr) -> String` lowers to:
//   Int    = 1 LLVM arg
//   String = 3 LLVM args (guts + payload + discrim)
//   Bool   = 1 LLVM arg
// = 5 LLVM arg slots for 3 SIL formal params. The wrapper's loop runs 3 times
// and consumes only 3 LLVM slots, mis-attributing the remaining slots to
// swiftself / ind_error.

enum LargeErr: Error {
  case boom(Int, Int, Int, Int)
}

func runMulti<T, Failure: Error>(
  _ a: Int, _ b: String, _ c: Bool,
  _ body: (Int, String, Bool) async throws(Failure) -> T
) async -> Result<T, Failure> {
  do { return .success(try await body(a, b, c)) }
  catch { return .failure(error) }
}

@main
struct Main {
  static func main() async {
    let r = await runMulti(42, "hello", true) {
      (x, y, z) async throws(LargeErr) -> String in
      "x=\(x) y=\(y) z=\(z)"
    }
    switch r {
    case .success(let s): print("ok: \(s)")
    case .failure(let e): print("err: \(e)")
    }
    // CHECK: ok: x=42 y=hello z=true
  }
}
