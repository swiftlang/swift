// RUN: %target-typecheck-verify-swift

struct TupleStruct<First, @_typeSequence Rest> {
  var first: First
  var rest: Rest
}

func debugPrint<@_typeSequence T>(_ items: T...)
  where T: CustomDebugStringConvertible
{
  /*for (item: T) in items {
    stdout.write(item.debugDescription)
  }*/
}

func max<@_typeSequence T>(_ values: T...) -> T?
  where T: Comparable
{
  return nil
}

func min<@_typeSequence T: Comparable>(_ values: T...) -> T? {
  return nil
}

func badParameter<T>(_ : @_typeSequence T) {} // expected-error {{attribute does not apply to type}}
