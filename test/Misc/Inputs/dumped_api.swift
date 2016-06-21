
public class _AnyIteratorBase {
}

/// An abstract `IteratorProtocol` base class over `T` elements.
///
/// Use this as a `Sequence`'s associated `Iterator` type when you
/// don't want to expose details of the concrete iterator, a subclass.
///
/// It is an error to create instances of `AnyIterator` that are not
/// also instances of an `AnyIterator` subclass.
///
/// See also:
///
///     struct AnySequence<S: Sequence>
///     func anyIterator<I: IteratorProtocol>(base: I) -> AnyIterator<I.Element>
///     func anyIterator<T>(nextImplementation: () -> T?) -> AnyIterator<T>
public class AnyIterator<T> : _AnyIteratorBase, IteratorProtocol {

  /// Initialize the instance.  May only be called from a subclass
  /// initializer.
  override public init()

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Note: subclasses must override this method.
  public func next() -> T?
}

/// Every `IteratorProtocol` can also be a `Sequence`.  Note that
/// traversing the sequence consumes the iterator.
extension AnyIterator : Sequence {

  /// Returns `self`.
  public func makeIterator() -> AnyIterator
}
/// Return a `IteratorProtocol` instance that wraps `base` but whose type
/// depends only on the type of `I.Element`.
///
/// Example:
///
///     func countStrings() -> AnyIterator<String> {
///       let lazyStrings = lazy(0..<10).map { String($0) }
///
///       // This is a really complicated type of no interest to our
///       // clients.
///       let g: MapSequenceIterator<RangeIterator<Int>, String>
///         = lazyStrings.makeIterator()
///       return anyIterator(g)
///     }
public func anyIterator<I: IteratorProtocol>(base: I) -> AnyIterator<I.Element>

public class FooIteratorBox<
  Base: IteratorProtocol
> : AnyIterator<Base.Element> {

  public override func next() -> Base.Element?
}
