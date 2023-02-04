
@_moveOnly
public struct MoveOnlyStruct {
  private let desc: Int

  public init(desc: Int) {
    self.desc = desc
  }

  deinit { }
}

@_moveOnly
public enum MoveOnlyEnum {
    case lhs(Int)
    case rhs(Int)

    deinit { }
}
