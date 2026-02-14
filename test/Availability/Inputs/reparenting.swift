#if IncludeNewProto
@available(macOS 12, *)
@reparentable public protocol NewProto {
  associatedtype Thing: Equatable
  func new() -> Thing
}

@available(macOS 12, *)
extension Existing: @reparented NewProto where Thing == String {
  public func new() -> Thing { return "defaulted Existing.new" }
}

public protocol Existing: NewProto {
  associatedtype Thing: Equatable = String
  func existing() -> Thing
}
#else
public protocol Existing {
  associatedtype Thing: Equatable = String
  func existing() -> Thing
}
#endif

public protocol Derived: Existing {
  func derived() -> Thing
}


public func libraryFunc(_ s: some Derived) {
  print("libraryFunc start")

  print(s.derived())
  print(s.existing())

#if IncludeNewProto
  if #available(macOS 12, *) { // FIXME: this doesn't properly guard the block.
    print(s.new())
  }
#endif

  print("libraryFunc end")
}
