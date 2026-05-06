
@reparentable
public protocol A {
  func helloA()
}
@reparentable
public protocol B {
  func helloB()
}

public protocol C {
  func helloC()
}

@reparentable
public protocol D {
  func helloD()
}

public protocol MultiParent: A, B, C, D {}
