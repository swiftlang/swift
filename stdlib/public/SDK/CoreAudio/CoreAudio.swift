//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import CoreAudio // Clang module

extension UnsafeBufferPointer {
  /// Initialize an `UnsafeBufferPointer<Element>` from an `AudioBuffer`.
  public init(_ audioBuffer: AudioBuffer) {
    self.init(
      start: UnsafePointer<Element>(audioBuffer.mData),
      length: Int(audioBuffer.mDataByteSize) / strideof(Element))
  }
}

extension UnsafeMutableBufferPointer {
  /// Initialize an `UnsafeMutableBufferPointer<Element>` from an
  /// `AudioBuffer`.
  public init(_ audioBuffer: AudioBuffer) {
    self.init(
      start: UnsafeMutablePointer<Element>(audioBuffer.mData),
      length: Int(audioBuffer.mDataByteSize) / strideof(Element))
  }
}

extension AudioBuffer {
  /// Initialize an `AudioBuffer` from an
  /// `UnsafeMutableBufferPointer<Element>`.
  public init<Element>(
    _ typedBuffer: UnsafeMutableBufferPointer<Element>,
    numberOfChannels: Int
  ) {
    self.mNumberChannels = UInt32(numberOfChannels)
    self.mData = UnsafeMutablePointer<Void>(typedBuffer.baseAddress)
    self.mDataByteSize = UInt32(typedBuffer.length * strideof(Element))
  }
}

extension AudioBufferList {
  /// - Returns: the size in bytes of an `AudioBufferList` that can hold up to
  ///   `maximumBuffers` `AudioBuffer`s.
  public static func sizeInBytes(maximumBuffers maximumBuffers: Int) -> Int {
    _require(maximumBuffers >= 1,
      "AudioBufferList should contain at least one AudioBuffer")
    return sizeof(AudioBufferList) +
      (maximumBuffers - 1) * strideof(AudioBuffer)
  }

  /// Allocate an `AudioBufferList` with a capacity for the specified number of
  /// `AudioBuffer`s.
  ///
  /// The `length` property of the new `AudioBufferList` is initialized to
  /// `maximumBuffers`.
  ///
  /// The memory should be freed with `free()`.
  public static func allocate(maximumBuffers maximumBuffers: Int)
    -> UnsafeMutableAudioBufferListPointer {
    let lengthInBytes = sizeInBytes(maximumBuffers: maximumBuffers)
    let ablMemory = calloc(lengthInBytes, 1)
    _require(ablMemory != nil,
      "failed to allocate memory for an AudioBufferList")

    let abl = UnsafeMutableAudioBufferListPointer(
        UnsafeMutablePointer<AudioBufferList>(ablMemory))
    abl.length = maximumBuffers
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

  /// The number of `AudioBuffer`s in the `AudioBufferList`
  /// (`mNumberBuffers`).
  public var length: Int {
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
    return UnsafeMutablePointer<AudioBuffer>(unsafeMutablePointer + 1) - 1
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

extension UnsafeMutableAudioBufferListPointer : MutableCollection {
  /// Always zero, which is the index of the first `AudioBuffer`.
  public var startIndex: Int {
    return 0
  }

  /// The "past the end" position; always identical to `length`.
  public var endIndex: Int {
    return length
  }

  /// Access an indexed `AudioBuffer` (`mBuffers[i]`).
  public subscript(index: Int) -> AudioBuffer {
    get {
      _require(index >= 0 && index < self.length,
        "subscript index out of range")
      return (_audioBuffersPointer + index).pointee
    }
    nonmutating set(newValue) {
      _require(index >= 0 && index < self.length,
        "subscript index out of range")
      (_audioBuffersPointer + index).pointee = newValue
    }
  }
}

