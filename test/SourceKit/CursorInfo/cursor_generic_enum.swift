// Checks that SourceKit does not crash for enum patterns in generics.
// RUN: %sourcekitd-test -req cursor -pos 18:15 %s -- %s | %FileCheck %s
// CHECK: source.lang.swift.ref.enumelement (12:10-12:17)

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
