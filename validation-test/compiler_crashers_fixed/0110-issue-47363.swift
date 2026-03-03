// RUN: not %target-swift-frontend %s -typecheck

// https://github.com/apple/swift/issues/47363

public protocol _UTFEncoding {
    associatedtype EncodedScalar where EncodedScalar == Int
}

public protocol UnicodeEncoding {
    associatedtype EncodedScalar: BidirectionalCollection
}

public protocol _UTFParser {
    associatedtype Encoding: UnicodeEncoding, _UTFEncoding
}
