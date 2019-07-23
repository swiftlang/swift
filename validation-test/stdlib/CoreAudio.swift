// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import StdlibCollectionUnittest


import CoreAudio

// Used in tests below.
extension AudioBuffer : Equatable {}

public func == (lhs: AudioBuffer, rhs: AudioBuffer) -> Bool {
  return lhs.mNumberChannels == rhs.mNumberChannels &&
    lhs.mDataByteSize == rhs.mDataByteSize &&
    lhs.mData == rhs.mData
}

var CoreAudioTestSuite = TestSuite("CoreAudio")

// The size of the non-flexible part of an AudioBufferList.
#if arch(i386) || arch(arm)
let ablHeaderSize = 4
#elseif arch(x86_64) || arch(arm64)
let ablHeaderSize = 8
#endif

CoreAudioTestSuite.test("UnsafeBufferPointer.init(_: AudioBuffer)") {
  do {
    let audioBuffer = AudioBuffer(
      mNumberChannels: 0, mDataByteSize: 0, mData: nil)
    let result: UnsafeBufferPointer<Float> = UnsafeBufferPointer(audioBuffer)
    expectEqual(nil, result.baseAddress)
    expectEqual(0, result.count)
  }

  do {
    let audioBuffer = AudioBuffer(
      mNumberChannels: 2, mDataByteSize: 1024,
      mData: UnsafeMutableRawPointer(bitPattern: 0x1234_5678))
    let result: UnsafeBufferPointer<Float> = UnsafeBufferPointer(audioBuffer)
    expectEqual(audioBuffer.mData, UnsafeRawPointer(result.baseAddress!))
    expectEqual(256, result.count)
  }
}

CoreAudioTestSuite.test("UnsafeMutableBufferPointer.init(_: AudioBuffer)") {
  do {
    let audioBuffer = AudioBuffer(
      mNumberChannels: 0, mDataByteSize: 0, mData: nil)
    let result: UnsafeMutableBufferPointer<Float> =
      UnsafeMutableBufferPointer(audioBuffer)
    expectEqual(nil, result.baseAddress)
    expectEqual(0, result.count)
  }

  do {
    let audioBuffer = AudioBuffer(
      mNumberChannels: 2, mDataByteSize: 1024,
      mData: UnsafeMutableRawPointer(bitPattern: 0x1234_5678))
    let result: UnsafeMutableBufferPointer<Float> =
      UnsafeMutableBufferPointer(audioBuffer)
    expectEqual(audioBuffer.mData!, UnsafeMutableRawPointer(result.baseAddress!))
    expectEqual(256, result.count)
  }
}

CoreAudioTestSuite.test(
  "AudioBuffer.init(_: UnsafeMutableBufferPointer, numberOfChannels: Int)") {
  do {
    // NULL pointer.
    let buffer = UnsafeMutableBufferPointer<Float>(start: nil, count: 0)
    let result = AudioBuffer(buffer, numberOfChannels: 2)
    expectEqual(2, result.mNumberChannels)
    expectEqual(0, result.mDataByteSize)
    expectEqual(nil, result.mData)
  }
  do {
    // Non-NULL pointer.
    let buffer = UnsafeMutableBufferPointer<Float>(
      start: UnsafeMutablePointer<Float>(bitPattern: 0x1234_5678), count: 0)
    let result = AudioBuffer(buffer, numberOfChannels: 2)
    expectEqual(2, result.mNumberChannels)
    expectEqual(0, result.mDataByteSize)
    expectEqual(buffer.baseAddress, result.mData)
  }
}

CoreAudioTestSuite.test(
  "AudioBuffer.init(_: UnsafeMutableBufferPointer, numberOfChannels: Int)/trap") {
#if arch(i386) || arch(arm)
  let overflowingCount = Int.max
#elseif arch(x86_64) || arch(arm64)
  let overflowingCount = Int(UInt32.max)
#endif
  let buffer = UnsafeMutableBufferPointer<Float>(
    start: UnsafeMutablePointer<Float>(bitPattern: 0x1234_5678),
    count: overflowingCount)

  expectCrashLater()
  // An overflow happens when we try to compute the value for mDataByteSize.
  _ = AudioBuffer(buffer, numberOfChannels: 2)
}

CoreAudioTestSuite.test("AudioBufferList.sizeInBytes(maximumBuffers: Int)") {
  expectEqual(ablHeaderSize + MemoryLayout<AudioBuffer>.stride,
    AudioBufferList.sizeInBytes(maximumBuffers: 1))
  expectEqual(ablHeaderSize + 16 * MemoryLayout<AudioBuffer>.stride,
    AudioBufferList.sizeInBytes(maximumBuffers: 16))
}

CoreAudioTestSuite.test("AudioBufferList.sizeInBytes(maximumBuffers: Int)/trap/count<0") {
  expectCrashLater()
  _ = AudioBufferList.sizeInBytes(maximumBuffers: -1)
}

CoreAudioTestSuite.test("AudioBufferList.sizeInBytes(maximumBuffers: Int)/trap/count==0") {
  expectCrashLater()
  _ = AudioBufferList.sizeInBytes(maximumBuffers: -1)
}

CoreAudioTestSuite.test("AudioBufferList.sizeInBytes(maximumBuffers: Int)/trap/overflow") {
  expectCrashLater()
  _ = AudioBufferList.sizeInBytes(maximumBuffers: Int.max)
}

CoreAudioTestSuite.test("AudioBufferList.allocate(maximumBuffers: Int)") {
  do {
    let ablPtrWrapper = AudioBufferList.allocate(maximumBuffers: 1)
    expectEqual(1, ablPtrWrapper.count)
    free(ablPtrWrapper.unsafeMutablePointer)
  }
  do {
    let ablPtrWrapper = AudioBufferList.allocate(maximumBuffers: 16)
    expectEqual(16, ablPtrWrapper.count)
    free(ablPtrWrapper.unsafeMutablePointer)
  }
}

CoreAudioTestSuite.test("AudioBufferList.allocate(maximumBuffers: Int)/trap/count==0") {
  expectCrashLater()
  _ = AudioBufferList.allocate(maximumBuffers: 0)
}

CoreAudioTestSuite.test("AudioBufferList.allocate(maximumBuffers: Int)/trap/count<0") {
  expectCrashLater()
  _ = AudioBufferList.allocate(maximumBuffers: -1)
}

CoreAudioTestSuite.test("AudioBufferList.allocate(maximumBuffers: Int)/trap/overflow") {
  expectCrashLater()
  _ = AudioBufferList.allocate(maximumBuffers: Int.max)
}

CoreAudioTestSuite.test("UnsafeMutableAudioBufferListPointer/AssociatedTypes") {
  typealias Subject = UnsafeMutableAudioBufferListPointer
  expectRandomAccessCollectionAssociatedTypes(
    collectionType: Subject.self,
    iteratorType: IndexingIterator<Subject>.self,
    subSequenceType: Slice<Subject>.self,
    indexType: Int.self,
    indicesType: CountableRange<Int>.self)
}

CoreAudioTestSuite.test(
  "UnsafeMutableAudioBufferListPointer.init(_: UnsafeMutablePointer<AudioBufferList>)," +
  "UnsafeMutableAudioBufferListPointer.unsafePointer," +
  "UnsafeMutableAudioBufferListPointer.unsafeMutablePointer") {
  do {
    let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(nil)
    expectNil(ablPtrWrapper)
  }

  do {
    let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(
      UnsafeMutablePointer<AudioBufferList>(bitPattern: 0x1234_5678)!)
    expectEqual(
      UnsafePointer<AudioBufferList>(bitPattern: 0x1234_5678),
      ablPtrWrapper.unsafePointer)
    expectEqual(
      UnsafePointer<AudioBufferList>(bitPattern: 0x1234_5678),
      ablPtrWrapper.unsafeMutablePointer)
  }

  do {
    let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(
      UnsafeMutablePointer<AudioBufferList>(bitPattern: 0x1234_5678))
    expectNotNil(ablPtrWrapper)
    expectEqual(
      UnsafePointer<AudioBufferList>(bitPattern: 0x1234_5678),
      ablPtrWrapper!.unsafePointer)
    expectEqual(
      UnsafePointer<AudioBufferList>(bitPattern: 0x1234_5678),
      ablPtrWrapper!.unsafeMutablePointer)
  }
}

CoreAudioTestSuite.test("UnsafeMutableAudioBufferListPointer.count") {
  let sizeInBytes = AudioBufferList.sizeInBytes(maximumBuffers: 16)
  let rawPtr = UnsafeMutableRawPointer.allocate(
    byteCount: sizeInBytes, alignment: MemoryLayout<AudioBufferList>.alignment)
  let ablPtr = rawPtr.bindMemory(to: AudioBufferList.self,
    capacity: sizeInBytes / MemoryLayout<AudioBufferList>.stride)

  // It is important that 'ablPtrWrapper' is a 'let'.  We are verifying that
  // the 'count' property has a nonmutating setter.
  let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(ablPtr)

  // Test getter.
  rawPtr.storeBytes(of: 0x1234_5678, as: UInt32.self)
  expectEqual(0x1234_5678, ablPtrWrapper.count)

  // Test setter.
  ablPtrWrapper.count = 0x7765_4321
  expectEqual(0x7765_4321, rawPtr.load(as: UInt32.self))

  rawPtr.deallocate()
}

CoreAudioTestSuite.test("UnsafeMutableAudioBufferListPointer.subscript(_: Int)") {
  let sizeInBytes = AudioBufferList.sizeInBytes(maximumBuffers: 16)

  let rawPtr = UnsafeMutableRawPointer.allocate(
    byteCount: sizeInBytes, alignment: 1)

  let ablPtr = rawPtr.bindMemory(
    to: AudioBufferList.self,
    capacity: sizeInBytes / MemoryLayout<AudioBufferList>.stride)

  // It is important that 'ablPtrWrapper' is a 'let'.  We are verifying that
  // the subscript has a nonmutating setter.
  let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(ablPtr)

  do {
    // Test getter.
    let audioBuffer = AudioBuffer(
      mNumberChannels: 2, mDataByteSize: 1024,
      mData: UnsafeMutableRawPointer(bitPattern: 0x1234_5678))

    let bufPtr = (rawPtr + ablHeaderSize).assumingMemoryBound(
      to: AudioBuffer.self)
    bufPtr.pointee = audioBuffer
    ablPtrWrapper.count = 1

    expectEqual(2, ablPtrWrapper[0].mNumberChannels)
    expectEqual(1024, ablPtrWrapper[0].mDataByteSize)
    expectEqual(audioBuffer.mData, ablPtrWrapper[0].mData)
  }

  do {
    // Test setter.
    let audioBuffer = AudioBuffer(
      mNumberChannels: 5, mDataByteSize: 256,
      mData: UnsafeMutableRawPointer(bitPattern: 0x8765_4321 as UInt))

    ablPtrWrapper.count = 2
    ablPtrWrapper[1] = audioBuffer

    let audioBufferPtr = (rawPtr + ablHeaderSize).assumingMemoryBound(
      to: AudioBuffer.self) + 1
    expectEqual(5, audioBufferPtr.pointee.mNumberChannels)
    expectEqual(256, audioBufferPtr.pointee.mDataByteSize)
    expectEqual(audioBuffer.mData, audioBufferPtr.pointee.mData)
  }

  ablPtr.deallocate()
}

CoreAudioTestSuite.test("UnsafeMutableAudioBufferListPointer.subscript(_: Int)/trap") {
  let ablPtrWrapper = AudioBufferList.allocate(maximumBuffers: 4)

  ablPtrWrapper[0].mNumberChannels = 42
  ablPtrWrapper[1].mNumberChannels = 42
  ablPtrWrapper[2].mNumberChannels = 42
  ablPtrWrapper[3].mNumberChannels = 42

  expectCrashLater()
  ablPtrWrapper[4].mNumberChannels = 42
}

CoreAudioTestSuite.test("UnsafeMutableAudioBufferListPointer/Collection") {
  var ablPtrWrapper = AudioBufferList.allocate(maximumBuffers: 16)
  expectType(UnsafeMutableAudioBufferListPointer.self, &ablPtrWrapper)

  var expected: [AudioBuffer] = []
  for i in 0..<16 {
    let audioBuffer = AudioBuffer(
      mNumberChannels: UInt32(2 + i), mDataByteSize: UInt32(1024 * i),
      mData: UnsafeMutableRawPointer(bitPattern: 0x1234_5678 + i * 10))

    ablPtrWrapper[i] = audioBuffer
    expected.append(audioBuffer)
  }

  // FIXME: use checkMutableRandomAccessCollection, when we have that function.
  checkRandomAccessCollection(expected, ablPtrWrapper)
  free(ablPtrWrapper.unsafeMutablePointer)
}

runAllTests()
