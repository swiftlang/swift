protocol FooBaseProto {}

protocol FooProto: FooBaseProto {}

protocol BarProto {
  associatedtype Foo: FooProto
  init(foo: Foo)
}

protocol BazProto {
  associatedtype Bar: BarProto
  init(bar: Bar)
}

struct BarImpl: BarProto {
  init(foo: FooImpl) {}
}
