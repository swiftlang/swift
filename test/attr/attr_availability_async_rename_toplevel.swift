// REQUIRES: concurrency

// RUN: %target-typecheck-verify-swift

if #available(macOS 12.0, *) {
  @available(*, renamed: "process(data:)")
  func process(data: [Int], completion: @escaping ([Int]) -> Void) { completion(data) }
  // expected-note@+1{{'process(data:)' declared here}}
  func process(data: [Int]) async -> [Int] { return data }

  func asyncFunc(data: [Int]) async {
    defer {
      process(data: data, completion: { print($0) })
    }

    func b() {
      process(data: data, completion: { print($0) })
    }
    // expected-warning@+1{{consider using asynchronous alternative function}}
    process(data: data, completion: { print($0) })
  }
}
