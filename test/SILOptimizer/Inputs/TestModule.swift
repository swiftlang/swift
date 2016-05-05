
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
private func callit(_ p: Proto) {
}

@_transparent
public func testit(_ n: MyStruct) {
  callit(n)
}
