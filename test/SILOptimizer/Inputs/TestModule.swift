
@usableFromInline
protocol Proto {
  func confx()
}

public struct MyStruct : Proto {
  @usableFromInline
  func confx() {
  }

  public init() {
  }
}

@inline(never)
@usableFromInline
func callit(_ p: Proto) {
}

@_transparent
public func testit(_ n: MyStruct) {
  callit(n)
}
