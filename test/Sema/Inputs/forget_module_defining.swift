@_moveOnly
public struct Regular {
  private let sorry = 0
}

@_moveOnly
@frozen public struct Frozen {
  private let lotfan = 0
}


public extension Regular {
  __consuming func endParty() {
    _forget self
  }
}

public extension Frozen {
  __consuming func endParty() {
    _forget self
  }
}
