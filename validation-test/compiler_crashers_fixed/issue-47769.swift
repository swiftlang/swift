// RUN: not %target-swift-frontend -typecheck %s

struct _MyError: ExpressibleByIntegerLiteral {
    typealias IntegerLiteralType = Int

    private var _underlyingValue: _MyError.IntegerLiteralType
    var httpErrorCode: UInt = 0

    public init(integerLiteral value: _MyError.IntegerLiteralType) {
        _underlyingValue = value
    }
}

enum MyError: _MyError, Error {
    typealias RawValue = Int
    case caseOne = 0
}
