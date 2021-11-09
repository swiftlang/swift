// RUN: %target-swift-frontend -emit-ir %s
// REQUIRES: concurrency

private struct TransformResult<T> {
    var index: Int
    var transformedElement: T

    init(index: Int, transformedElement: T) {
        self.index = index
        self.transformedElement = transformedElement
    }
}

public extension Collection {
    @available(SwiftStdlib 5.1, *)
    private func f<T>(_ transform: @escaping (Element) async throws -> T) async throws -> [T] {
        return try await withThrowingTaskGroup(of: TransformResult<T>.self) { group in
          return []
        }
    }
}
