struct A {
  private(set) var x: Int {
    get { return 0 }
    set {}
  }

  private(set) subscript(x: Int) -> Int {
    get { return x }
    set {}
  }
}
