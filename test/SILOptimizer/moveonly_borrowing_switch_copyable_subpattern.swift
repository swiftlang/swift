// RUN: %target-swift-frontend -emit-sil -verify %s

struct Payload: ~Copyable {
    var x: Int
    var y: String
}

enum Foo: ~Copyable {
    case nonCopyablePayload(Payload)
    case copyablePayload(String)
}

func hungryCondition(_: consuming String) -> Bool { fatalError() }
func condition(_: borrowing String) -> Bool { fatalError() }

func eat(_: consuming String) {}
func nibble(_: borrowing String) {}

func test(borrowing foo: borrowing Foo) {
    switch foo {
    case .nonCopyablePayload(let x): // expected-warning{{}}
        break

    // OK to form a normal `let` binding when the payload is copyable.
    // Then it's OK to consume copies of it in the condition clause
    // and in the body.
    case .copyablePayload(let x) where hungryCondition(x):
        eat(x)
        nibble(x)
    case .copyablePayload(let x) where condition(x):
        eat(x)
        nibble(x)

    case .copyablePayload(let x):
        nibble(x)
    }
}
