// RUN: %target-swift-frontend -emit-ir -g -primary-file %s

public struct TestType<T: Error> { }

extension Array: Error where Element: Error { }

public struct GenericType<G> {
    public func test<T>(_ value: TestType<[T]>) { }
}
