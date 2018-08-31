//===----------------------------------------------------------------------===//
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

@_exported import CoreAudio // Clang module

extension UnsafeBufferPointer {
  /// Initialize an `UnsafeBufferPointer<Element>` from an `AudioBuffer`.
  /// Binds the buffer's memory type to `Element`.
  public init(_ audioBuffer: AudioBuffer) {
    let count = Int(audioBuffer.mDataByteSize) / MemoryLayout<Element>.stride
    let elementPtr = audioBuffer.mData?.bindMemory(
      to: Element.self, capacity: count)
    self.init(start: elementPtr, count: count)
  }
}

extension UnsafeMutableBufferPointer {
  /// Initialize an `UnsafeMutableBufferPointer<Element>` from an
  /// `AudioBuffer`.
  public init(_ audioBuffer: AudioBuffer) {
    let count = Int(audioBuffer.mDataByteSize) / MemoryLayout<Element>.stride
    let elementPtr = audioBuffer.mData?.bindMemory(
      to: Element.self, capacity: count)
    self.init(start: elementPtr, count: count)
  }
}

extension AudioBuffer {
  /// Initialize an `AudioBuffer` from an
  /// `UnsafeMutableBufferPointer<Element>`.
  public init<Element>(
    _ typedBuffer: UnsafeMutableBufferPointer<Element>,
    numberOfChannels: Int
  ) {
    let byteSize = typedBuffer.count * MemoryLayout<Element>.stride
    self.init(mNumberChannels: UInt32(numberOfChannels),
              mDataByteSize: UInt32(byteSize),
              mData: UnsafeMutableRawPointer(typedBuffer.baseAddress))
  }
}

extension AudioBufferList {
  /// - Returns: the size in bytes of an `AudioBufferList` that can hold up to
  ///   `maximumBuffers` `AudioBuffer`s.
  public static func sizeInBytes(maximumBuffers: Int) -> Int {
    precondition(maximumBuffers >= 1,
      "AudioBufferList should contain at least one AudioBuffer")
    return MemoryLayout<AudioBufferList>.size +
      (maximumBuffers - 1) * MemoryLayout<AudioBuffer>.stride
  }

  /// Allocate an `AudioBufferList` with a capacity for the specified number of
  /// `AudioBuffer`s.
  ///
  /// The `count` property of the new `AudioBufferList` is initialized to
  /// `maximumBuffers`.
  ///
  /// The memory should be freed with `free()`.
  public static func allocate(maximumBuffers: Int)
    -> UnsafeMutableAudioBufferListPointer {
    let byteSize = sizeInBytes(maximumBuffers: maximumBuffers)
    let ablMemory = calloc(byteSize, 1)
    precondition(ablMemory != nil,
      "failed to allocate memory for an AudioBufferList")

    let listPtr = ablMemory!.bindMemory(to: AudioBufferList.self, capacity: 1)
    (ablMemory! + MemoryLayout<AudioBufferList>.stride).bindMemory(
      to: AudioBuffer.self, capacity: maximumBuffers)
    let abl = UnsafeMutableAudioBufferListPointer(listPtr)
    abl.count = maximumBuffers
    return abl
  }
}

/// A wrapper for a pointer to an `AudioBufferList`.
///
/// Like `UnsafeMutablePointer`, this type provides no automated memory
/// management and the user must therefore take care to allocate and free
/// memory appropriately.
public struct UnsafeMutableAudioBufferListPointer {
  /// Construct from an `AudioBufferList` pointer.
  public init(_ p: UnsafeMutablePointer<AudioBufferList>) {
    unsafeMutablePointer = p
  }

  /// Construct from an `AudioBufferList` pointer.
  public init?(_ p: UnsafeMutablePointer<AudioBufferList>?) {
    guard let unwrapped = p else { return nil }
    self.init(unwrapped)
  }

  /// The number of `AudioBuffer`s in the `AudioBufferList`
  /// (`mNumberBuffers`).
  public var count: Int {
    get {
      return Int(unsafeMutablePointer.pointee.mNumberBuffers)
    }
    nonmutating set(newValue) {
      unsafeMutablePointer.pointee.mNumberBuffers = UInt32(newValue)
    }
  }

  /// The pointer to the first `AudioBuffer` in this `AudioBufferList`.
  internal var _audioBuffersPointer: UnsafeMutablePointer<AudioBuffer> {
    // AudioBufferList has one AudioBuffer in a "flexible array member".
    // Position the pointer after that, and skip one AudioBuffer back.  This
    // brings us to the start of AudioBuffer array.
    let rawPtr = UnsafeMutableRawPointer(unsafeMutablePointer + 1)
    return rawPtr.assumingMemoryBound(to: AudioBuffer.self) - 1
  }

  // FIXME: the properties 'unsafePointer' and 'unsafeMutablePointer' should be
  // initializers on UnsafePointer and UnsafeMutablePointer, but we don't want
  // to allow any UnsafePointer<Element> to be initializable from an
  // UnsafeMutableAudioBufferListPointer, only UnsafePointer<AudioBufferList>.
  // We need constrained extensions for that.  rdar://17821143

  /// The pointer to the wrapped `AudioBufferList`.
  public var unsafePointer: UnsafePointer<AudioBufferList> {
    return UnsafePointer(unsafeMutablePointer)
  }

  /// The pointer to the wrapped `AudioBufferList`.
  public var unsafeMutablePointer: UnsafeMutablePointer<AudioBufferList>
}

extension UnsafeMutableAudioBufferListPointer
  : MutableCollection, RandomAccessCollection {

  public typealias Element = AudioBuffer
  public typealias Index = Int
  public typealias Indices = Range<Int>

  /// Always zero, which is the index of the first `AudioBuffer`.
  public var startIndex: Int {
    return 0
  }

  /// The "past the end" position; always identical to `count`.
  public var endIndex: Int {
    return count
  }

  /// Access an indexed `AudioBuffer` (`mBuffers[i]`).
  public subscript(index: Index) -> Element {
    get {
      precondition(index >= 0 && index < self.count,
        "subscript index out of range")
      return (_audioBuffersPointer + index).pointee
    }
    nonmutating set(newValue) {
      precondition(index >= 0 && index < self.count,
        "subscript index out of range")
      (_audioBuffersPointer + index).pointee = newValue
    }
  }
}

