// RUN: %target-swift-frontend %s -emit-silgen

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
