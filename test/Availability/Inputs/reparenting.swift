@available(macOS 12, *)
@reparentable public protocol NewProto {
  associatedtype Thing: Equatable
  func new() -> Thing
}

public protocol Existing: NewProto {
  associatedtype Thing: Equatable = String
  func existing() -> Thing
}

public protocol Derived: Existing {
  func derived() -> Thing
}

@available(macOS 12, *)
extension Existing: @reparented NewProto where Thing == String {
  public func new() -> Thing { return "defaulted Existing.new" }
}


public func libraryFunc(_ s: some Derived) {
  print("libraryFunc start")
  if #available(macOS 12, *) {
    // FIXME: this doesn't work due to "referencing instance method 'new()' on 'Existing' requires the types '(some Derived).Thing' and 'String' be equivalent"
    // rdar://170187513
    // print(s.new())
    print("defaulted Existing.new")
  }
  print("libraryFunc end")
}
