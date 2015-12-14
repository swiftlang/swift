
private protocol Proto {
  func confx()
}

public struct MyStruct : Proto {
  func confx() {
  }

  public init() {
  }
}

@inline(never)
private func callit(p: Proto) {
}

@_transparent
public func testit(n: MyStruct) {
  callit(n)
}
