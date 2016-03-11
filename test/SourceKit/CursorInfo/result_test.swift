// RUN: %sourcekitd-test -req cursor -pos 17:15 %s -- %s >%t.response
// RUN: diff -u %s.response %t.response

public protocol MyErrorType {
}

enum MyErrorEnum: MyErrorType {
}

public enum ResultTest<Value, Error: MyErrorType> {
    case Success(Value)
    case Failure(Error)

    /// Returns `true` if the result is a success, `false` otherwise.
    public var isSuccess: Bool {
        switch self {
        case .Success:
            return true
        case .Failure:
            return false
        }
    }
}
