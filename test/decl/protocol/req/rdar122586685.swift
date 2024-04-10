// RUN: %target-typecheck-verify-swift

public struct S: P {}

public protocol P: Collection {}

extension P {
    public func index(after i: Int) -> Int { fatalError() }
    public var startIndex: Int { fatalError() }
    public var endIndex: Int { fatalError() }
    public subscript(index: Int) -> String { fatalError() }
    public func makeIterator() -> AnyIterator<String> { fatalError() }
}

let x: AnyIterator<String>.Type = S.Iterator.self
