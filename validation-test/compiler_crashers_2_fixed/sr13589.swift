// RUN: %target-swift-emit-silgen %s

// https://github.com/apple/swift/issues/56024

public var UNIMPLEMENTED: Never { fatalError("UNIMPLEMENTED") }

public func UNIMPLEMENTED(
    function: String = #function,
    file: StaticString = #file,
    line: UInt = #line
) -> Never {
    fatalError("UNIMPLEMENTED ← \(function)", file: file, line: line)
}

public struct Test {
    public func foo() throws -> UnkeyedDecodingContainer {
        UNIMPLEMENTED()
    }
}
