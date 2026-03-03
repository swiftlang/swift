// RUN: %target-swift-frontend -emit-ir %s

public typealias OnValue<Value> = () -> Value

public protocol SubjectProtocol<Value> {
    associatedtype Value

    func value() -> Value
    func onValue(_ onValue: @escaping OnValue<Value>) -> any SubjectProtocol<Value>
}

class Subject<Value>: SubjectProtocol {
    var _onValue: OnValue<Value>?

    init(_ onValue: OnValue<Value>?) {
        self._onValue = onValue
    }

    func value() -> Value {
        if let onValue = self._onValue {
            return onValue()
        } else {
            fatalError("Require Implementation")
        }
    }

    func onValue(_ onValue: @escaping OnValue<Value>) -> any SubjectProtocol<Value> {
        let subject = Subject(self._onValue)
        subject._onValue = onValue

        return subject
    }
}

public func ExistentialSubject<Value>() -> any SubjectProtocol<Value> {
    Subject(nil)
}

public func DebugSubject() {
    ExistentialSubject().onValue { 0 }
}

