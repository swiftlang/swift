// RUN: %target-swift-frontend %s -emit-silgen

// https://github.com/apple/swift/issues/45800

protocol CType {
    init()
}

protocol BType {
    associatedtype C: CType
}

protocol A {
    associatedtype B: BType
    typealias C = B.C
}

func test<T: A>(_ a: T) -> T.C {
    return T.C()
}
