public extension Regular {
  __consuming func shutdownParty() {
    _forget self // ok; same module
  }
}

public extension Frozen {
  __consuming func shutdownParty() {
    _forget self // ok; same module
  }
}
