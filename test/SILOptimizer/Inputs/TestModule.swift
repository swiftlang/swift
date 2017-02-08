
@_versioned
protocol Proto {
  func confx()
}

public struct MyStruct : Proto {
  func confx() {
  }

  public init() {
  }
}

@inline(never)
@_versioned
func callit(_ p: Proto) {
}

@_transparent
public func testit(_ n: MyStruct) {
  callit(n)
}
