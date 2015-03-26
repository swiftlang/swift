// RUN: %target-parse-verify-swift

// ----------------------------------------------------------------------------
// Using protocol requirements from inside protocol extensions
// ----------------------------------------------------------------------------
protocol P1 {
  func reqP1a() -> Bool
}

extension P1 {
  func extP1a() -> Bool { return !reqP1a() }

  var extP1b: Bool {
    return self.reqP1a()
  }

  var extP1c: Bool {
    return extP1b && self.extP1a()
  }
}

protocol P2 {
  typealias AssocP2 : P1

  func reqP2a() -> AssocP2
}

extension P2 {
  func extP2a() -> AssocP2? { return reqP2a() }

  func extP2b() {
    self.reqP2a().reqP1a()
  }

  func extP2c() -> Self.AssocP2 { return extP2a()! }
}

protocol P3 {
  typealias AssocP3 : P2

  func reqP3a() -> AssocP3
}

extension P3 {
  func extP3a() -> AssocP3.AssocP2 {
    return reqP3a().reqP2a()
  }
}

// ----------------------------------------------------------------------------
// Using generics from inside protocol extensions
// ----------------------------------------------------------------------------
func acceptsP1<T : P1>(t: T) { }

extension P1 {
  func extP1d() { acceptsP1(self) }
}

func acceptsP2<T : P2>(t: T) { }

extension P2 {
  func extP2acceptsP1() { acceptsP1(reqP2a()) }
  func extP2acceptsP2() { acceptsP2(self) }
}
