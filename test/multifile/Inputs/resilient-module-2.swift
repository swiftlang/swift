public enum ResilientType {
  case a(Int64)
  case b(Int64)
}

@frozen
public enum SomeEnum {
  case first(ResilientType)
  case second(ResilientType)
}
