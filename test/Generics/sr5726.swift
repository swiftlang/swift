// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

// SR-5726
protocol Foo
{
  associatedtype Bar
  var bar: Bar { get }
}

extension Foo where Self: Collection, Bar: Collection, Self.SubSequence == Bar.SubSequence, /*redundant: */Self.Index == Bar.Index // expected-warning {{redundant same-type constraint 'Self.Index' == 'Self.Bar.Index'}}
{
  subscript(_ bounds: Range<Index>) -> SubSequence
  {
    return bar[bounds]
  }
}
