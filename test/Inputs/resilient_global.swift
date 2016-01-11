public struct EmptyResilientStruct {
  public init() {}

  public var computed: Int {
    return 1337
  }
}

public var emptyGlobal = EmptyResilientStruct()
