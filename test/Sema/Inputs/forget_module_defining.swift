public struct Regular : ~Copyable {
  private let sorry = 0
}

@frozen public struct Frozen : ~Copyable {
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
