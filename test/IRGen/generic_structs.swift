// RUN: %target-swift-frontend -disable-type-layout -primary-file %s -emit-ir

struct A<T1, T2>
{
  var b: T1
  var c: T2
  var d: B<T1, T2>
}
struct B<T1, T2>
{
  var c: T1
  var d: T2
}

struct C<T1>
{}
struct D<T2>
{}

struct Foo<A1, A2>
{
  var a: A1
  var b: Bar<A1, A2>
}

struct Bar<A1, A2> {
}

public protocol Proto { }

public struct EmptyStruct {}

public struct GenericStruct<T : Proto> {
  var empty: EmptyStruct = EmptyStruct()
  var dummy: Int = 0
  var opt: Optional<T> = nil

  public init() {}
}
