// RUN: %target-swift-frontend -module-name TestModule -typecheck -verify %s

struct SendableType {}
class NonSendableType {
    init() {}
}
struct SendableTypeWithTypeParam<SendableType> {}

func testSendingOnNonSendable(input: sending NonSendableType) -> sending NonSendableType {
    return NonSendableType() // okay
}

func testSendingOnSendableParam(input: sending SendableType) {
    // expected-warning@-1{{'sending' has no effect}}{{40-48=}}
}

func testSendingOnSendable(input: sending SendableTypeWithTypeParam<SendableType>) {
    // expected-warning@-1{{'sending' has no effect}}{{35-43=}}
}

func testSendingOnSendableParam(input: sending some Sendable) {
    // expected-warning@-1{{'sending' has no effect}}{{40-48=}}
}

func testSendingOnSendableParam(input: sending any Sendable) {
    // expected-warning@-1{{'sending' has no effect}}{{40-48=}}
}

func testSendingOnSendableResult() -> sending SendableType {
    // expected-warning@-1{{'sending' has no effect}}{{39-47=}}
    return SendableType()
}
