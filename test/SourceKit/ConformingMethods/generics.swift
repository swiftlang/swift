protocol Proto {}
struct ConcreteProto : Proto {}
struct ConcreteProtoGen<T> : Proto {}

struct S<T> : Proto {
  func methodForProto1(x: T) -> ConcreteProto {}
  func methodForProto2<U>(x: U) -> ConcreteProtoGen<U> {}
  func methodForProto3(x: Self) -> ConcreteProtoGen<T> {}
  func methodForProto4() -> Self {}
  func methodForInt() -> Int { return 1 }
  mutating func test() {
    self.
  }
}

func test<X>(value: S<X>) {
  value.
}

// RUN: %sourcekitd-test -req=conformingmethods -pos=12:10 %s -req-opts=expectedtypes='$s8MyModule5ProtoPD' -- -module-name MyModule %s > %t.response.1
// RUN: %diff -u %s.response.1 %t.response.1
// RUN: %sourcekitd-test -req=conformingmethods -pos=17:8 %s -req-opts=expectedtypes='$s8MyModule5ProtoPD' -- -module-name MyModule %s > %t.response.2
// RUN: %diff -u %s.response.2 %t.response.2
