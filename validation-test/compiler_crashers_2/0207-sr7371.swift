// RUN: not --crash %target-swift-frontend -emit-ir %s
// rdar://problem/65241930
// UNSUPPORTED: asan

public protocol TypedParserResultTransferType {
    // Remove type constraint
    associatedtype Result: ParserResult
}

public struct AnyTypedParserResultTransferType<P: ParserResult>: TypedParserResultTransferType {
    public typealias Result = P
    // Remove property
    public let result: P
}

public protocol ParserResult {}
public protocol StaticParser: ParserResult {}

// Change comformance to ParserResult
public protocol TypedStaticParser: StaticParser {
    // Remove type constraint
    associatedtype ResultTransferType: TypedParserResultTransferType
}

// Remove where clause
public protocol MutableSelfStaticParser: TypedStaticParser where ResultTransferType == AnyTypedParserResultTransferType<Self> {
    func parseTypeVar() -> AnyTypedParserResultTransferType<Self>
}

extension MutableSelfStaticParser {

    public func anyFunction() -> () {
        let t = self.parseTypeVar
        // Remove this and below
        _ = t()
        _ = self.parseTypeVar()
    }
}
