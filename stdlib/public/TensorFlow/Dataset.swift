//===-- Dataset.swift -----------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The dataset API.
//
// NOTE: `SingleValueDataset`, `DoubleValueDataset`, etc are interfaces
// duplicated for different arities, and they are so because of limitations of
// the type system and the `#tfop` interface. While the future direction is to
// have one `Dataset<Element>` type to unify all datasets, e.g.
// `Dataset<(Tensor<Float>, Tensor<Int32>)>`, Arbitrary arity Dataset types are
// not possible until two new features of `#tfop` are implemented. Please refer
// to SR-8573 and SR-8574 for more details.
//
//===----------------------------------------------------------------------===//

/// The default graph seed.
///
/// - Note: See TensorFlow's `python.framework.random_seed.DEFAULT_GRAPH_SEED`.
@usableFromInline let _defaultGraphSeed: Int64 = 87654321

/// Returns the local seeds an operation should use given an op-specific seed.
///
/// Given operation-specific seed, `seed`, this helper function returns two
/// seeds derived from graph-level and op-level seeds. Many random operations
/// internally use the two seeds to allow user to change the seed globally for a
/// graph, or for only specific operations.
///
/// - Note: See TensorFlow's `python.framework.random_seed.get_seed`.
///
// TODO: There's no support for TF's "global seed" yet, so we always use the
// default graph seed as the first seed. Need to investigate the best way to
// model TF's "global seed".
@usableFromInline @inline(__always)
func _tensorSeeds(_ seed: Tensor<Int64>) -> (Tensor<Int64>, Tensor<Int64>) {
  return (Tensor(_defaultGraphSeed), seed)
}

//===----------------------------------------------------------------------===//
// Single value dataset
//===----------------------------------------------------------------------===//

/// Represents a potentially large set of elements.
///
/// A `SingleValueDataset` can be used to represent an input pipeline as a
/// collection of element tensors.
@_fixed_layout
public struct SingleValueDataset<ScalarOfElement : AccelerableByTensorFlow> {
  @usableFromInline let _handle: VariantHandle

  @usableFromInline @inline(__always)
  internal init(_handle: VariantHandle) {
    self._handle = _handle
  }
}

public extension SingleValueDataset {
  @inlinable @inline(__always)
  init(randomSeed: Int64) {
    let (seed1, seed2) = _tensorSeeds(Tensor(randomSeed))
    self.init(
      _handle: #tfop("RandomDataset", seed1, seed2,
                     output_types: [ScalarOfElement.self],
                     output_shapes: [nil] as [TensorShape?])
    )
  }
}

public extension SingleValueDataset {
  /// Creates a dataset from a batch of elements as a tensor.
  @inlinable @inline(__always)
  public init(elements: Tensor<ScalarOfElement>) {
    // A dataset creation op only runs on TF CPU.
    self.init(
      _handle: #tfop(
        "TensorSliceDataset", [elements],
        Toutput_types: [ScalarOfElement.self],
        output_shapes: [nil] as [TensorShape?]
      )
    )
  }
}

extension SingleValueDataset : Sequence {
  public typealias Element = Tensor<ScalarOfElement>

  public typealias Iterator = SingleValueDatasetIterator<ScalarOfElement>

  /// Returns an iterator over the elements of this dataset.
  @inlinable @inline(__always)
  public func makeIterator() -> SingleValueDatasetIterator<ScalarOfElement> {
    let resource: ResourceHandle = #tfop("AnonymousIterator",
                                         output_types: [ScalarOfElement.self],
                                         output_shapes: [nil] as [TensorShape?])
    #tfop("MakeIterator", _handle, resource) as Void
    return SingleValueDatasetIterator(_handle: resource)
  }
}

public extension SingleValueDataset {
  @inlinable @inline(__always)
  func map<T : AccelerableByTensorFlow>(
    _ transform: @convention(tensorflow) (Tensor<ScalarOfElement>) -> Tensor<T>
  ) -> SingleValueDataset<T> {
    // FIXME(SR-8570): If we pass an empty Array<TensorHandle<T>> as
    // other_arguments and an empty Array<Any.Type> as Targuments, partitioning
    // won't recognize the attribute:
    //    error: unknown array attribute
    return SingleValueDataset<T>(
      _handle: #tfop(
        "MapDataset", _handle, [Tensor<Int32>(0)], f: transform,
        Targuments: [Int32.self],
        output_types: [T.self], output_shapes: [nil] as [TensorShape?]
      )
    )
  }

  @inlinable @inline(__always)
  func filter(
    _ isIncluded:
      @convention(tensorflow) (Tensor<ScalarOfElement>) -> Tensor<Bool>
  ) -> SingleValueDataset {
    // FIXME(SR-8570): If we pass an empty Array<TensorHandle<T>> as
    // other_arguments and an empty Array<Any.Type> as Targuments, partitioning
    // won't recognize the attribute:
    //    error: unknown array attribute
    return SingleValueDataset(
      _handle: #tfop(
        "FilterDataset", _handle, [Tensor<Int32>(0)],
        predicate: isIncluded, Targuments: [Int32.self],
        output_types: [ScalarOfElement.self], output_shapes: [nil] as [TensorShape?]
      )
    )
  }

  @inlinable @inline(__always)
  func shuffled(
    sampleCount: Int64, randomSeed: Int64
  ) -> SingleValueDataset {
    let (seed1, seed2) = _tensorSeeds(Tensor(randomSeed))
    return SingleValueDataset(
      _handle: #tfop(
        "ShuffleDataset", _handle, Tensor<Int64>(sampleCount), seed1, seed2,
        output_types: [ScalarOfElement.self], output_shapes: [nil] as [TensorShape?]
      )
    )
  }

  @inlinable @inline(__always)
  func batched(_ batchSize: Int64) -> SingleValueDataset {
    return SingleValueDataset(
      _handle: #tfop(
        "BatchDataset", _handle, Tensor<Int64>(batchSize),
        output_types: [ScalarOfElement.self], output_shapes: [nil] as [TensorShape?]
      )
    )
  }
}

/// Represents the state of iterating through a `SingleValueDataset`.
@_fixed_layout
public struct SingleValueDatasetIterator<ScalarOfElement>
  where ScalarOfElement : AccelerableByTensorFlow {
  @usableFromInline let handle: ResourceHandle

  @usableFromInline @inline(__always)
  internal init(_handle: ResourceHandle) {
    self.handle = _handle
  }
}

extension SingleValueDatasetIterator : IteratorProtocol {
  public typealias Element = Tensor<ScalarOfElement>

  // Advances to the next element and returns it, or nil if no next element
  // exists.
  @inlinable @inline(__always)
  public mutating func next() -> Element? {
    let optional: VariantHandle =
      #tfop("IteratorGetNextAsOptional", handle,
            output_types: [ScalarOfElement.self], output_shapes: [nil] as [TensorShape?])
    guard _TFGetScalarOrDie(#tfop("OptionalHasValue", optional)) else {
      return nil
    }
    return Tensor(handle: #tfop("OptionalGetValue", optional,
                                output_types: [ScalarOfElement.self],
                                output_shapes: [nil] as [TensorShape?]))
  }
}

//===----------------------------------------------------------------------===//
// Double value dataset
//===----------------------------------------------------------------------===//

/// Represents a potentially large set of elements.
///
/// A `DoubleValueDataset` can be used to represent an input pipeline as a
/// collection of element tensor tuples.
// FIXME: SR-8599 blocks heterogeneous scalar types.
@_fixed_layout
public struct DoubleValueDataset<ScalarOfFirstElement>
  where ScalarOfFirstElement : AccelerableByTensorFlow {
  public typealias ScalarOfSecondElement = ScalarOfFirstElement
  @usableFromInline let _handle: VariantHandle

  @usableFromInline @inline(__always)
  internal init(_handle: VariantHandle) {
    self._handle = _handle
  }
}

public extension DoubleValueDataset {
  @inlinable @inline(__always)
  init(randomSeed: Int64) {
    let (seed1, seed2) = _tensorSeeds(Tensor(randomSeed))
    self.init(
      _handle: #tfop("RandomDataset", seed1, seed2,
                     output_types: [ScalarOfFirstElement.self,
                                    ScalarOfSecondElement.self],
                     output_shapes: [nil, nil] as [TensorShape?])
    )
  }
}

public extension DoubleValueDataset {
  /// Creates a dataset from a batch of elements as a tensor.
  @inlinable @inline(__always)
  public init(elements: (Tensor<ScalarOfFirstElement>,
                         Tensor<ScalarOfSecondElement>)) {
    // A dataset creation op only runs on TF CPU.
    self.init(
      _handle: #tfop(
        "TensorSliceDataset", [elements.0.handle, elements.1.handle],
        Toutput_types: [ScalarOfFirstElement.self, ScalarOfSecondElement.self],
        output_shapes: [nil, nil] as [TensorShape?]
      )
    )
  }
}

extension DoubleValueDataset : Sequence {
  public typealias Element = (Tensor<ScalarOfFirstElement>,
                              Tensor<ScalarOfSecondElement>)

  public typealias Iterator =
    DoubleValueDatasetIterator<ScalarOfFirstElement>

  /// Returns an iterator over the elements of this dataset.
  @inlinable @inline(__always)
  public func makeIterator()
    -> DoubleValueDatasetIterator<ScalarOfFirstElement> {
    let resource: ResourceHandle =
      #tfop("AnonymousIterator",
            output_types: [ScalarOfFirstElement.self,
                           ScalarOfSecondElement.self],
            output_shapes: [nil, nil] as [TensorShape?])
    #tfop("MakeIterator", _handle, resource) as Void
    return DoubleValueDatasetIterator(_handle: resource)
  }
}

public extension DoubleValueDataset {
  @inlinable @inline(__always)
  func map<T : AccelerableByTensorFlow>(
    _ transform:
      @convention(tensorflow) (Tensor<ScalarOfFirstElement>,
                               Tensor<ScalarOfSecondElement>)
        -> (Tensor<T>, Tensor<T>)
  ) -> DoubleValueDataset<T> {
    // FIXME(SR-8570): If we pass an empty Array<TensorHandle<T>> as
    // other_arguments and an empty Array<Any.Type> as Targuments, partitioning
    // won't recognize the attribute:
    //    error: unknown array attribute
    return DoubleValueDataset<T>(
      _handle: #tfop(
        "MapDataset", _handle, [Tensor<Int32>(0)], f: transform,
        Targuments: [Int32.self],
        output_types: [T.self, T.self],
        output_shapes: [nil, nil] as [TensorShape?]
      )
    )
  }

  @inlinable @inline(__always)
  func filter(
    _ isIncluded:
      @convention(tensorflow) (Tensor<ScalarOfFirstElement>,
                               Tensor<ScalarOfSecondElement>) -> Tensor<Bool>
  ) -> DoubleValueDataset {
    // FIXME(SR-8570): If we pass an empty Array<TensorHandle<T>> as
    // other_arguments and an empty Array<Any.Type> as Targuments, partitioning
    // won't recognize the attribute:
    //    error: unknown array attribute
    return DoubleValueDataset(
      _handle: #tfop(
        "FilterDataset", _handle, [Tensor<Int32>(0)],
        predicate: isIncluded, Targuments: [Int32.self],
        output_types: [ScalarOfFirstElement.self, ScalarOfSecondElement.self],
        output_shapes: [nil, nil] as [TensorShape?]
      )
    )
  }

  @inlinable @inline(__always)
  func shuffled(
    sampleCount: Int64, randomSeed: Int64
  ) -> DoubleValueDataset {
    let (seed1, seed2) = _tensorSeeds(Tensor(randomSeed))
    return DoubleValueDataset(
      _handle: #tfop(
        "ShuffleDataset", _handle, Tensor<Int64>(sampleCount), seed1, seed2,
        output_types: [ScalarOfFirstElement.self, ScalarOfSecondElement.self],
        output_shapes: [nil, nil] as [TensorShape?]
      )
    )
  }

  @inlinable @inline(__always)
  func batched(_ batchSize: Int64) -> DoubleValueDataset {
    return DoubleValueDataset(
      _handle: #tfop(
        "BatchDataset", _handle, Tensor<Int64>(batchSize),
        output_types: [ScalarOfFirstElement.self, ScalarOfSecondElement.self],
        output_shapes: [nil, nil] as [TensorShape?]
      )
    )
  }
}

/// Represents the state of iterating through a `DoubleValueDataset`.
// FIXME: SR-8599 blocks heterogeneous scalar types.
@_fixed_layout
public struct DoubleValueDatasetIterator<ScalarOfFirstElement>
  where ScalarOfFirstElement : AccelerableByTensorFlow {
  public typealias ScalarOfSecondElement = ScalarOfFirstElement
  @usableFromInline let handle: ResourceHandle

  @usableFromInline @inline(__always)
  internal init(_handle: ResourceHandle) {
    self.handle = _handle
  }
}

extension DoubleValueDatasetIterator : IteratorProtocol {
  public typealias Element = (Tensor<ScalarOfFirstElement>,
                              Tensor<ScalarOfSecondElement>)

  // Advances to the next element and returns it, or nil if no next element
  // exists.
  @inlinable @inline(__always)
  public mutating func next() -> Element? {
    let optional: VariantHandle =
      #tfop("IteratorGetNextAsOptional", handle,
            output_types: [ScalarOfFirstElement.self,
                           ScalarOfSecondElement.self],
            output_shapes: [nil, nil] as [TensorShape?])
    guard _TFGetScalarOrDie(#tfop("OptionalHasValue", optional)) else {
      return nil
    }
    let (firstHandle, secondHandle): (TensorHandle<ScalarOfFirstElement>,
                                      TensorHandle<ScalarOfSecondElement>) =
      #tfop("OptionalGetValue", optional,
            output_types: [ScalarOfFirstElement.self,
                           ScalarOfSecondElement.self],
            output_shapes: [nil, nil] as [TensorShape?])
    return (Tensor(handle: firstHandle), Tensor(handle: secondHandle))
  }
}

@inlinable @inline(__always)
// FIXME: SR-8599 blocks heterogeneous scalar types.
public func zip<T>(
  _ first: SingleValueDataset<T>,
  _ second: SingleValueDataset<T>
) -> DoubleValueDataset<T> {
  return DoubleValueDataset(
    _handle: #tfop("ZipDataset", [first._handle, second._handle],
                   output_types: [T.self, T.self],
                   output_shapes: [nil, nil] as [TensorShape?],
                   N: 2)
  )
}
