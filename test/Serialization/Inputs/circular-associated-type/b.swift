public protocol BB {
  associatedtype T
}

public protocol B {
  associatedtype T : BB
}
