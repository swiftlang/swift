protocol FooProt {
  var instanceVar: Int { get }
}

struct FooStruct : FooProt {
  var instanceVar: Int = 17
  func instanceFunc0() {}

  private func privateFunc_ERROR() {}
}

struct GenericFooStruct<T> : FooProt {
  var instanceVar: Int = 17
  func instanceFunc0() {}

  private func privateFunc_ERROR() {}
}
