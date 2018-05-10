public struct Box {
  public lazy var x: Int = {
    return true ? 0 : 1
  }()

  public init() {}
}
