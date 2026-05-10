// RUN: %target-swift-frontend -emit-sil %s

// https://github.com/apple/swift/issues/50711

protocol SignalInterface {
	associatedtype OutputValue
}

class Signal<OV>: SignalInterface {
  typealias OutputValue = OV
}

extension Signal {
  func foo<U>(_: U) -> SignalChannel<[U], Signal<Array<U>>>
    where OutputValue == Optional<U> { return SignalChannel() }
}

struct SignalChannel<OutputValue, Output: Signal<OutputValue>> { }

