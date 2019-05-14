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
/// A `Dataset` can be used to represent an input pipeline as a collection of
/// element tensors.
@_fixed_layout
public struct Dataset<Element : TensorGroup> {
  public let _handle: VariantHandle

  @inlinable
  public init(_handle: VariantHandle) {
    self._handle = _handle
  }
}

public extension Dataset {
  @inlinable
  init(randomSeed: Int64) {
    let (seed1, seed2) = _tensorSeeds(Tensor(randomSeed))
    self.init(_handle: Raw.experimentalRandomDataset(
      seed: seed1,
      seed2: seed2,
      outputTypes: Element._typeList,
      outputShapes: Element._unknownShapeList))
  }
}

public extension Dataset {
  /// Creates a dataset from a batch of elements as a tensor.
  @inlinable
  init(elements: Element) {
    self.init(_handle: Raw.tensorSliceDataset(
      components: [elements],
      outputShapes: Element._unknownShapeList))
  }
}

extension Dataset : Sequence {
  public typealias Iterator = DatasetIterator<Element>

  /// Returns an iterator over the elements of this dataset.
  @inlinable
  public func makeIterator() -> DatasetIterator<Element> {
    let resource = Raw.anonymousIterator(
      outputTypes: Element._typeList,
      outputShapes: Element._unknownShapeList)
    Raw.makeIterator(dataset: _handle, iterator: resource)
    return DatasetIterator(_handle: resource)
  }
}

public extension Dataset {
  // Note that this Dataset API implementation uses an experimental tracing
  // feature, which is not robust and does not have great diagnostics yet.
  @inlinable
  func map<ResultElement : TensorGroup>(
    _ transform: (Element) -> ResultElement
  ) -> Dataset<ResultElement> {
    return Dataset<ResultElement>(_handle: Raw.mapDataset(
      inputDataset: _handle,
      otherArguments: Tensor<Int32>(0),
      f: transform,
      outputTypes: ResultElement._typeList,
      outputShapes: ResultElement._unknownShapeList,
      useInterOpParallelism: true,
      preserveCardinality: false))
  }

  @inlinable
  func map<ResultElement : TensorGroup>(
    parallelCallCount: Int,
    _ transform: (Element) -> ResultElement
  ) -> Dataset<ResultElement> {
    return Dataset<ResultElement>(_handle: Raw.parallelMapDataset(
      inputDataset: _handle,
      otherArguments: Tensor<Int32>(0),
      numParallelCalls: Tensor<Int32>(Int32(parallelCallCount)),
      f: transform,
      outputTypes: ResultElement._typeList,
      outputShapes: ResultElement._unknownShapeList,
      useInterOpParallelism: true,
      sloppy: false,
      preserveCardinality: false))
  }

  @inlinable
  func filter(
    _ isIncluded: (Element) -> Tensor<Bool>
  ) -> Dataset {
    return Dataset(_handle: Raw.filterDataset(
      inputDataset: _handle,
      otherArguments: Tensor<Int32>(0),
      predicate: isIncluded,
      outputTypes: Element._typeList,
      outputShapes: Element._unknownShapeList))
  }
}

public extension Dataset {
  @inlinable
  func shuffled(
    sampleCount: Int, randomSeed: Int64
  ) -> Dataset {
    let (seed1, seed2) = _tensorSeeds(Tensor(randomSeed))
    return Dataset(_handle: Raw.shuffleDataset(
      inputDataset: _handle,
      bufferSize: Tensor(Int64(sampleCount)),
      seed: seed1,
      seed2: seed2,
      outputTypes: Element._typeList,
      outputShapes: Element._unknownShapeList))
  }

  @inlinable
  func batched(_ batchSize: Int) -> Dataset {
    return Dataset(_handle: Raw.batchDataset(
      inputDataset: _handle,
      batchSize: Tensor(Int64(batchSize)),
      outputTypes: Element._typeList,
      outputShapes: Element._unknownShapeList))
  }
}

/// The type that allows iteration over a dataset's elements.
@_fixed_layout
public struct DatasetIterator<Element : TensorGroup> {
  @usableFromInline let _handle: ResourceHandle

  @usableFromInline
  internal init(_handle: ResourceHandle) {
    self._handle = _handle
  }
}

extension DatasetIterator : IteratorProtocol {
  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  @inlinable
  public mutating func next() -> Element? {
    let optional = Raw.iteratorGetNextAsOptional(
      iterator: _handle,
      outputTypes: Element._typeList,
      outputShapes: Element._unknownShapeList)
    guard Raw.optionalHasValue(optional: optional).scalarized() else {
      return nil
    }
    return Raw.optionalGetValue(
      optional: optional,
      outputShapes: Element._unknownShapeList)
  }
}

/// A 2-tuple-like struct that conforms to TensorGroup that represents a tuple 
/// of 2 types conforming to TensorGroup.
@_fixed_layout
public struct Zip2TensorGroup<T : TensorGroup, U : TensorGroup> : TensorGroup {
  public var first: T
  public var second: U

  public init(_ first: T, _ second: U) {
    self.first = first
    self.second = second
  }

  public var _handles: [_AnyTensorHandle] {
    var res: [_AnyTensorHandle] = first._handles
    res += second._handles
    return res
  }

  public init(_lazySingle op: TFE_Op, idx: Int32) {
    // TODO:
    self.first = T.init(_lazySingle: op, idx: idx)
    self.second = U.init(_lazySingle: op, idx: idx)
    assert(false)
  }
}

@inlinable
public func zip<T : TensorGroup, U : TensorGroup>(
  _ dataset1: Dataset<T>, _ dataset2: Dataset<U>
) -> Dataset<Zip2TensorGroup<T, U>> {
  let handle = Raw.zipDataset(
    inputDatasets: [dataset1._handle, dataset2._handle],
    outputTypes: Zip2TensorGroup<T, U>._typeList,
    outputShapes: Zip2TensorGroup<T, U>._unknownShapeList)
  return Dataset(_handle: handle)
}
