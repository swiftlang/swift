// Testing that these requests don't crash

public protocol Foo<Bar> {
  associatedtype Bar
}
// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s
typealias Problem = Foo<String>

protocol EmptyProto {}
// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s
typealias ConstrainedBar<T: EmptyProto> = Foo<T>
// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s
typealias ConstrainedBarMetatype<T: P> = Foo<T>.Type
