struct A {
  private(set) var x: Int {
    get { return 0 }
    set {}
  }

  private(set) subscript(x: Int) -> Int {
    get { return 0 }
    set {}
  }
}

func A_x_keypath() -> WritableKeyPath<A, Int> {
  return \A.x as! WritableKeyPath<A, Int>
}

func A_subscript_0_keypath() -> WritableKeyPath<A, Int> {
  return \A.[0] as! WritableKeyPath<A, Int>
}
