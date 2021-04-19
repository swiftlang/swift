open actor ResilientBaseActor {
  public init() {}
}

@_fixed_layout
open actor FixedSubclassOfResilientBaseActor : ResilientBaseActor {
  public override init() {}
}

@_fixed_layout
open actor FixedBaseActor {
  public init() {}
}
