struct A {
  dynamic var value : Int {
    return 1
  }
}

public func test() -> Int{
  return A().value
}
