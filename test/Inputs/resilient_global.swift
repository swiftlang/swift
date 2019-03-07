public struct EmptyResilientStruct {
  public init() {}

  public var computed: Int {
    return 1337
  }

  public mutating func mutate() {}
}

public var emptyGlobal = EmptyResilientStruct()

@_fixed_layout public var fixedLayoutGlobal = EmptyResilientStruct()
