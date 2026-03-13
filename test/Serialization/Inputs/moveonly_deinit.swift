
public struct MoveOnlyStruct: ~Copyable {
  private let desc: Int

  public init(desc: Int) {
    self.desc = desc
  }

  deinit { }
}

public enum MoveOnlyEnum: ~Copyable {
    case lhs(Int)
    case rhs(Int)

    deinit { }
}

public struct NormalStruct {
  private let desc: Int

  public init(desc: Int) {
    self.desc = desc
  }
}
