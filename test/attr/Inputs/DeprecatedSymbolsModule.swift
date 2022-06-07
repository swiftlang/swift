@available(*, deprecated)
public func importedDeprecatedFunc() {}

@available(*, deprecated)
public func importedDeprecatedFuncWithReturn() -> String {
    return "Donda"
}

@available(*, deprecated)
public var importedDeprecatedVar = "910210"

@available(*, deprecated)
public struct importedDeprecatedType {
    public var x = ""
    public static let m = 22

    public func memberFunc() -> Int {
        40
    }
}

@available(*, deprecated)
public protocol importedDeprecatedProtocol {
    var val: String { get }
}

public protocol ImprotedNonDeprecatedProtocol {
    var a: String { get }
}

// expected-warning {{'importedDeprecatedType' is deprecated}}
// expected-warning {{'importedDeprecatedProtocol' is deprecated}}
extension importedDeprecatedType: importedDeprecatedProtocol { 
    public var val: String {
        return "someValHere"
    }
}

@available(*, deprecated)
extension importedDeprecatedType: ImprotedNonDeprecatedProtocol {
    public var a: String {
        return "hello"
    }
}