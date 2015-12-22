
public class _AnyGeneratorBase {
}

/// An abstract `GeneratorType` base class over `T` elements.
///
/// Use this as a `Sequence`'s associated `Generator` type when you
/// don't want to expose details of the concrete generator, a subclass.
///
/// It is an error to create instances of `AnyGenerator` that are not
/// also instances of an `AnyGenerator` subclass.
///
/// See also:
///
///     struct AnySequence<S: SequenceType>
///     func anyGenerator<G: GeneratorType>(base: G) -> AnyGenerator<G.Element>
///     func anyGenerator<T>(nextImplementation: () -> T?) -> AnyGenerator<T>
public class AnyGenerator<T> : _AnyGeneratorBase, GeneratorType {
  /// Initialize the instance.  May only be called from a subclass
  /// initializer.
  override public init()
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Note: subclasses must override this method.
  public func next() -> T?
}

/// Every `GeneratorType` can also be a `SequenceType`.  Note that
/// traversing the sequence consumes the generator.
extension AnyGenerator : SequenceType {
  /// Returns `self`.
  public func generate() -> AnyGenerator
}
/// Return a `GeneratorType` instance that wraps `base` but whose type
/// depends only on the type of `G.Element`.
///
/// Example:
///
///     func countStrings() -> AnyGenerator<String> {
///       let lazyStrings = lazy(0..<10).map { String($0) }
///
///       // This is a really complicated type of no interest to our
///       // clients.
///       let g: MapSequenceGenerator<RangeGenerator<Int>, String>
///         = lazyStrings.generate()
///       return anyGenerator(g)
///     }
public func anyGenerator<G: GeneratorType>(base: G) -> AnyGenerator<G.Element>

public class FooGeneratorBox<
  Base: GeneratorType
> : AnyGenerator<Base.Element> {
  public override func next() -> Base.Element?
}
