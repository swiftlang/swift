// RUN: not %target-swift-frontend -emit-ir -primary-file %s

// REQUIRES: asserts

internal protocol _UTFEncoding {
    associatedtype EncodedScalar where EncodedScalar == Int
}

public protocol _UnicodeEncoding {
    associatedtype EncodedScalar : BidirectionalCollection
}

public protocol _UnicodeEncoding_ {
    associatedtype ForwardParser: _UnicodeEncoding
}

public protocol UnicodeEncoding: _UnicodeEncoding_ where ForwardParser == Self {}

public protocol _UTFParser {
    associatedtype Encoding: UnicodeEncoding, _UTFEncoding
}
