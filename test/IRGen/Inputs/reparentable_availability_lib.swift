@reparentable public protocol RP {}

public protocol P: RP {
  func doP()
}

@available(macOS 14, *)
extension P: @reparented RP {}
