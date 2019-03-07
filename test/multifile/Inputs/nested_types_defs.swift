struct Outer {
  struct Inner {
  }
}

struct Outer2 {
  enum InnerE {
  }
}

struct Outer3 {
  class InnerC {
  }
}

struct Outer4 {}

extension Outer4 {
  struct InnerExtension {}
}
