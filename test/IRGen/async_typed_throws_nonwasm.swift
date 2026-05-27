// RUN: %target-swift-frontend -primary-file %s \
// RUN:   -emit-ir -parse-as-library -disable-availability-checking \
// RUN:   | %FileCheck %s

// REQUIRES: concurrency
// UNSUPPORTED: CPU=wasm32
// UNSUPPORTED: CPU=wasm64

enum E: Error { case boom }

func run<T, Failure: Error>(
  _ body: () async throws(Failure) -> T
) async -> Result<T, Failure> {
  do { return .success(try await body()) }
  catch { return .failure(error) }
}

@main
struct Main {
  static func main() async {
    let r = await run { () async throws -> String in throw E.boom }
    _ = r
  }
}

// Off-wasm regression guard for the `TwasmA` thin-to-thick wrapper introduced
// for swiftlang/swift#89320.

// CHECK-LABEL: define {{(hidden|internal)}} {{(swiftcc|swifttailcc)}} void @"$s{{[^"]+}}4MainV4mainyyYaFZ"
// CHECK: call {{(swiftcc|swifttailcc)}} void @"$s{{[^"]+}}3runy{{[^"]+}}"({{.*}} ptr @"$s{{[^"]+}}fU_Tu",

// Wrapper symbols MUST NOT appear in the module on any non-wasm target.
// `TwasmA` matches both the wrapper definition and its Tu global.
// CHECK-NOT: TwasmA
