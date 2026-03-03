public struct Regular: ~Copyable {
  private let sorry = 0
  // FIXME: rdar://108933330 (cannot define struct deinit with -enable-library-evolution)
//  deinit {}
}

public extension Regular {
  __consuming func endParty() {
    // FIXME: rdar://108933330 (cannot define struct deinit with -enable-library-evolution)
//    discard self
  }
}

@frozen public struct Frozen: ~Copyable {
  private let lotfan = 0
  deinit {}
}

public extension Frozen {
  __consuming func endParty() {
    discard self
  }
}
