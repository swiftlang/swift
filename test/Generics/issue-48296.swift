// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/48296

protocol Foo
{
  associatedtype Bar
  var bar: Bar { get }
}

// CHECK: Generic signature: <Self where Self : Collection, Self : Foo, Self.[Foo]Bar : Collection, Self.[Collection]SubSequence == Self.[Foo]Bar.[Collection]SubSequence>
extension Foo where Self: Collection, Bar: Collection, Self.SubSequence == Bar.SubSequence, /*redundant: */Self.Index == Bar.Index {
  subscript(_ bounds: Range<Index>) -> SubSequence
  {
    return bar[bounds]
  }
}
