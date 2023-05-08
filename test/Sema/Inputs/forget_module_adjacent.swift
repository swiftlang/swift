public extension Regular {
  __consuming func shutdownParty() {
    // FIXME: rdar://108933330 (cannot define struct deinit with -enable-library-evolution)
//    _forget self // ok; same module
  }
}

public extension Frozen {
  __consuming func shutdownParty() {
    _forget self // ok; same module
  }
}
