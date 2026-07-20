// RUN: %target-swift-frontend -primary-file %s -emit-ir -parse-as-library \
// RUN:   -disable-availability-checking | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -parse-as-library \
// RUN:   -disable-availability-checking -enable-library-evolution | %FileCheck %s --check-prefix=RESILIENT

// REQUIRES: concurrency
// REQUIRES: OS=wasip1

struct LargeErr: Error {
  var tag: Int
  var pad: (Int, Int, Int, Int)
}

struct LargeResult {
  var tag: Int
  var pad: (Int, Int, Int, Int, Int, Int, Int, Int)
}

protocol PAsync {
  associatedtype Failure: Error
  associatedtype Output
  func boom() async throws(Failure) -> Output
}

struct ImplPAsync: PAsync {
  typealias Failure = LargeErr
  typealias Output = LargeResult
  func boom() async throws(LargeErr) -> LargeResult {
    throw LargeErr(tag: 1, pad: (0, 0, 0, 0))
  }
}

func run<T, Failure: Error>(
  _ body: () async throws(Failure) -> T
) async -> Result<T, Failure> {
  do { return .success(try await body()) }
  catch { return .failure(error) }
}

func runWitness<T: PAsync>(_ x: T) async -> Result<T.Output, T.Failure> {
  do { return .success(try await x.boom()) }
  catch { return .failure(error) }
}

@main
struct Main {
  static func main() async {
    let r = await run { () async throws(LargeErr) -> LargeResult in
      throw LargeErr(tag: 2, pad: (0, 0, 0, 0))
    }
    _ = r
    let w = await runWitness(ImplPAsync())
    _ = w
  }
}

// Witness thunk: trailing pair [indirect error, swiftself], then Self + WT.
// CHECK-LABEL: define internal {{(swifttailcc|swiftcc)}} void @"$s{{[^"]+}}P4boom6OutputQzyYa7FailureQzYKFTW"
// CHECK-SAME: (ptr noalias{{[^,]*}} %{{[0-9]+}}, ptr swiftasync %{{[0-9]+}}, ptr %{{[0-9]+}}, ptr noalias{{[^,]*}} swiftself{{[^,]*}} %{{[0-9]+}}, ptr %{{[0-9_a-zA-Z]+}}, ptr %{{[0-9_a-zA-Z]+}})

// Thin closure literal: indirect result, swiftasync, indirect error. No swiftself.
// CHECK-LABEL: define internal {{(swifttailcc|swiftcc)}} void @"$s{{[^"]+}}fU_"
// CHECK-SAME: (ptr noalias{{[^,]*}} %{{[0-9]+}}, ptr swiftasync %{{[0-9]+}}, ptr %{{[0-9]+}})

// The witness thunk must NOT use the old [..., swiftself, indirect error, Self, WT]
// layout (3 trailing ptrs after swiftself); the pair puts exactly 2 (Self + WT).
// CHECK-NOT: define internal {{(swifttailcc|swiftcc)}} void @"$s{{[^"]+}}P4boom6OutputQzyYa7FailureQzYKFTW"({{[^)]*}}swiftself{{[^,]*}}, ptr %{{[0-9]+}}, ptr %{{[0-9_a-zA-Z]+}}, ptr %{{[0-9_a-zA-Z]+}})

// Dispatch thunk (Tj) for the protocol requirement, emitted under library
// evolution. The entry carries [indirect error, swiftself] and the thunk must
// forward them in the same order (identity, not swapped).
// RESILIENT-LABEL: define{{.*}} {{(swifttailcc|swiftcc)}} void @"$s{{[^"]+}}PAsyncP4boom6OutputQzyYa7FailureQzYKFTj"
// RESILIENT-SAME: (ptr noalias{{[^,]*}} %{{[0-9]+}}, ptr swiftasync %{{[0-9]+}}, ptr %[[IE:[0-9]+]], ptr noalias{{[^,]*}} swiftself{{[^,]*}} %[[SS:[0-9]+]], ptr %{{[0-9_a-zA-Z]+}}, ptr %{{[0-9_a-zA-Z]+}})
// RESILIENT: call {{(swifttailcc|swiftcc)}} void %{{[0-9]+}}(ptr noalias{{[^,]*}} %{{[0-9]+}}, ptr swiftasync %{{[0-9]+}}, ptr %[[IE]], ptr noalias{{[^,]*}} swiftself{{[^,]*}} %[[SS]], ptr %{{[0-9_a-zA-Z]+}}, ptr %{{[0-9_a-zA-Z]+}})
