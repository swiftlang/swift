// RUN: %target-swift-frontend %s -emit-ir -g -o - -parse-as-library \
// RUN:    -module-name M  -target %target-swift-5.1-abi-triple
// REQUIRES: concurrency

// This used to crash the compiler. Just verify it builds.

@available(SwiftStdlib 5.1, *)
extension Collection where Element: Sendable {
  public func f() async throws {
    return try await withThrowingTaskGroup(of: Element.self) { group in
      var i = self.startIndex
      func doit() async throws {
        group.spawn { [i] in
          return self[i]
        }
      }
      try await doit()
    }
  }
}
