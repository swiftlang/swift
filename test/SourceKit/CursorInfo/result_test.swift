// RUN: %sourcekitd-test -req cursor -offset 441 %s -- -serialize-diagnostics -serialize-diagnostics-path %t.dia %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

import Foundation

public enum ResultTest<Value, Error: ErrorType> {
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
