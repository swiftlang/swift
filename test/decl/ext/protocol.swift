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

// ----------------------------------------------------------------------------
// Using generics from inside protocol extensions
// ----------------------------------------------------------------------------
func acceptsP1<T : P1>(t: T) { }

extension P1 {
  func extP1d() { acceptsP1(self) }
}
