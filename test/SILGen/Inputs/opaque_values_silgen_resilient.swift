public class ClassEquatable : Equatable {
  public static func ==(lhs: ClassEquatable, rhs: ClassEquatable) -> Bool { lhs === rhs }
}

public enum EnumNontrivialWithEmptyCases : Equatable {
  case empty
  case other
  case loaded(ClassEquatable)
}

public enum EnumTrivial : Equatable {
  case empty
  case other
}
