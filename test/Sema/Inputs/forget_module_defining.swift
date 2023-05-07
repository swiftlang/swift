@_moveOnly
public struct Regular {
  private let sorry = 0
  // FIXME: rdar://108933330 (cannot define struct deinit with -enable-library-evolution)
//  deinit {}
}

public extension Regular {
  __consuming func endParty() {
    // FIXME: rdar://108933330 (cannot define struct deinit with -enable-library-evolution)
//    _forget self
  }
}

@_moveOnly
@frozen public struct Frozen {
  private let lotfan = 0
  deinit {}
}

public extension Frozen {
  __consuming func endParty() {
    _forget self
  }
}
