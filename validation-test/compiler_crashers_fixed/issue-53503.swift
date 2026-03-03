// RUN: %target-swift-emit-silgen %s

// https://github.com/apple/swift/issues/53503

protocol Example {
    associatedtype Signed: SignedInteger
    associatedtype SP: StringProtocol
    var string: String { get }
}
extension Example {
    var string: String {
        return "Foo"
    }
}
class MyClass<T: SignedInteger, S: StringProtocol>: Example {
    typealias Signed = T
    typealias SP = S
}
extension MyClass where T == Int, S == String {
    var string: String {
        return "Bar"
    }
}

let myclass = MyClass<Int, String>()

print(myclass.string)
