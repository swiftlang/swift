// RUN: %target-swift-frontend -emit-ir -o %t.ll %s

// Just make sure we don't crash.

protocol Publicable {
    associatedtype PublicModel

    func publicized() -> PublicModel
}


protocol WithReturnType {
    associatedtype MainType
    associatedtype ReturnType

    func returnTheThing()
}

extension WithReturnType where MainType: Publicable {
    typealias ReturnType = MainType.PublicModel

    func returnTheThing() {
        print("publicable")
    }
}

extension WithReturnType {
    func returnTheThing() {
        print("not publicable")
    }
}

extension String: Publicable {
    struct PublicString {
        let inner: String

        init(str: String) {
            self.inner = "Public: \(str)"
        }
    }

    func publicized() -> PublicString {
        return PublicString(str: self)
    }
}

struct Controller<T> {

}

extension Controller: WithReturnType {
    typealias MainType = T
}

let controller = Controller<String>()

controller.returnTheThing()