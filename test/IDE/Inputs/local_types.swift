// Used to test merging of local types in print_local_types

public func defineDuplicateNamedLocalTypes() {
  struct S {
    let si: Int
  }
  class C {
    let ci: Int = 1
  }
  enum E {
    case One(Int)
  }
}
