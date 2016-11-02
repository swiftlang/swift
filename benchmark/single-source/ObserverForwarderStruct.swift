
class Observer {
  @inline(never)
  func receive(_ value: Int) {
  }
}

protocol Sink {
  func receive(_ value: Int)
}

struct Forwarder: Sink {
  let object: Observer

  func receive(_ value: Int) {
    object.receive(value)
  }
}

class Signal {
  var observers: [Sink] = []

  func subscribe(_ sink: Sink) {
    observers.append(sink)
  }

  func send(_ value: Int) {
    for observer in observers {
      observer.receive(value)
    }
  }
}

public func run_ObserverForwarderStruct(_ iterations: Int) {
  let signal = Signal()
  let observer = Observer()
  for _ in 0 ..< 10_000 * iterations {
    signal.subscribe(Forwarder(object: observer))
  }
  signal.send(1)
}
