// RUN: %target-typecheck-verify-swift

struct Row {}

// Force inference of Element first
typealias Foo<T: Sequence> = T.Element
typealias Bar = Foo<Row>

extension Row: Collection {
    public var startIndex: Int { 0 }

    public var endIndex: Int { 0 }

    public func index(after: Int) -> Int {
      fatalError()
    }

    // We should not be considering this as a candidate witness
    public subscript<T>(_: T) -> Any {
      fatalError()
    }

    // This is the one we want
    public subscript(position: Int) -> String {
      fatalError()
    }
}

// This should type check because Row.Element == String, and not Any
let x: Row.Element.Type = Row.Element.self
let y: String.Type = x
