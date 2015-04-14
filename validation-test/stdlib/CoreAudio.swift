// RUN: %target-run-simple-swift

// REQUIRES: objc_interop

import StdlibUnittest
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
  if true {
    let audioBuffer = AudioBuffer(
      mNumberChannels: 0, mDataByteSize: 0, mData: nil)
    let result: UnsafeBufferPointer<Float> = UnsafeBufferPointer(audioBuffer)
    expectEqual(nil, result.baseAddress)
    expectEqual(0, result.count)
  }

  if true {
    let audioBuffer = AudioBuffer(
      mNumberChannels: 2, mDataByteSize: 1024,
      mData: UnsafeMutablePointer<Void>(bitPattern: 0x1234_5678))
    let result: UnsafeBufferPointer<Float> = UnsafeBufferPointer(audioBuffer)
    expectEqual(
      UnsafePointer<Float>(audioBuffer.mData),
      result.baseAddress)
    expectEqual(256, result.count)
  }
}

CoreAudioTestSuite.test("UnsafeMutableBufferPointer.init(_: AudioBuffer)") {
  if true {
    let audioBuffer = AudioBuffer(
      mNumberChannels: 0, mDataByteSize: 0, mData: nil)
    let result: UnsafeMutableBufferPointer<Float> =
      UnsafeMutableBufferPointer(audioBuffer)
    expectEqual(nil, result.baseAddress)
    expectEqual(0, result.count)
  }

  if true {
    let audioBuffer = AudioBuffer(
      mNumberChannels: 2, mDataByteSize: 1024,
      mData: UnsafeMutablePointer<Void>(bitPattern: 0x1234_5678))
    let result: UnsafeMutableBufferPointer<Float> =
      UnsafeMutableBufferPointer(audioBuffer)
    expectEqual(
      UnsafeMutablePointer<Float>(audioBuffer.mData),
      result.baseAddress)
    expectEqual(256, result.count)
  }
}

CoreAudioTestSuite.test(
  "AudioBuffer.init(_: UnsafeMutableBufferPointer, numberOfChannels: Int)") {
  if true {
    // NULL pointer.
    let buffer = UnsafeMutableBufferPointer<Float>(start: nil, count: 0)
    let result = AudioBuffer(buffer, numberOfChannels: 2)
    expectEqual(2, result.mNumberChannels)
    expectEqual(0, result.mDataByteSize)
    expectEqual(nil, result.mData)
  }
  if true {
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
  AudioBuffer(buffer, numberOfChannels: 2)
}

CoreAudioTestSuite.test("AudioBufferList.sizeInBytes(maximumBuffers: Int)") {
  expectEqual(ablHeaderSize + strideof(AudioBuffer),
    AudioBufferList.sizeInBytes(maximumBuffers: 1))
  expectEqual(ablHeaderSize + 16 * strideof(AudioBuffer),
    AudioBufferList.sizeInBytes(maximumBuffers: 16))
}

CoreAudioTestSuite.test("AudioBufferList.sizeInBytes(maximumBuffers: Int)/trap/count<0") {
  expectCrashLater()
  AudioBufferList.sizeInBytes(maximumBuffers: -1)
}

CoreAudioTestSuite.test("AudioBufferList.sizeInBytes(maximumBuffers: Int)/trap/count==0") {
  expectCrashLater()
  AudioBufferList.sizeInBytes(maximumBuffers: -1)
}

CoreAudioTestSuite.test("AudioBufferList.sizeInBytes(maximumBuffers: Int)/trap/overflow") {
  expectCrashLater()
  AudioBufferList.sizeInBytes(maximumBuffers: Int.max)
}

CoreAudioTestSuite.test("AudioBufferList.allocate(maximumBuffers: Int)") {
  if true {
    let ablPtrWrapper = AudioBufferList.allocate(maximumBuffers: 1)
    expectEqual(1, ablPtrWrapper.count)
    free(ablPtrWrapper.unsafeMutablePointer)
  }
  if true {
    let ablPtrWrapper = AudioBufferList.allocate(maximumBuffers: 16)
    expectEqual(16, ablPtrWrapper.count)
    free(ablPtrWrapper.unsafeMutablePointer)
  }
}

CoreAudioTestSuite.test("AudioBufferList.allocate(maximumBuffers: Int)/trap/count==0") {
  expectCrashLater()
  AudioBufferList.allocate(maximumBuffers: 0)
}

CoreAudioTestSuite.test("AudioBufferList.allocate(maximumBuffers: Int)/trap/count<0") {
  expectCrashLater()
  AudioBufferList.allocate(maximumBuffers: -1)
}

CoreAudioTestSuite.test("AudioBufferList.allocate(maximumBuffers: Int)/trap/overflow") {
  expectCrashLater()
  AudioBufferList.allocate(maximumBuffers: Int.max)
}


CoreAudioTestSuite.test(
  "UnsafeMutableAudioBufferListPointer.init(_: UnsafeMutablePointer<AudioBufferList>)," +
  "UnsafeMutableAudioBufferListPointer.unsafePointer," +
  "UnsafeMutableAudioBufferListPointer.unsafeMutablePointer") {
  if true {
    let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(nil)
    expectEqual(nil, ablPtrWrapper.unsafePointer)
    expectEqual(nil, ablPtrWrapper.unsafeMutablePointer)
  }

  if true {
    let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(
      UnsafeMutablePointer<AudioBufferList>(bitPattern: 0x1234_5678))
    expectEqual(
      UnsafePointer<AudioBufferList>(bitPattern: 0x1234_5678),
      ablPtrWrapper.unsafePointer)
    expectEqual(
      UnsafePointer<AudioBufferList>(bitPattern: 0x1234_5678),
      ablPtrWrapper.unsafeMutablePointer)
  }
}

CoreAudioTestSuite.test("UnsafeMutableAudioBufferListPointer.count") {
  let sizeInBytes = AudioBufferList.sizeInBytes(maximumBuffers: 16)
  let ablPtr = UnsafeMutablePointer<AudioBufferList>(
    UnsafeMutablePointer<UInt8>.alloc(sizeInBytes))

  // It is important that 'ablPtrWrapper' is a 'let'.  We are verifying that
  // the 'count' property has a nonmutating setter.
  let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(ablPtr)

  // Test getter.
  UnsafeMutablePointer<UInt32>(ablPtr).memory = 0x1234_5678
  expectEqual(0x1234_5678, ablPtrWrapper.count)

  // Test setter.
  ablPtrWrapper.count = 0x7765_4321
  expectEqual(0x7765_4321, UnsafeMutablePointer<UInt32>(ablPtr).memory)

  ablPtr.dealloc(sizeInBytes)
}

CoreAudioTestSuite.test("UnsafeMutableAudioBufferListPointer.subscript(_: Int)") {
  let sizeInBytes = AudioBufferList.sizeInBytes(maximumBuffers: 16)
  let ablPtr = UnsafeMutablePointer<AudioBufferList>(
    UnsafeMutablePointer<UInt8>.alloc(sizeInBytes))

  // It is important that 'ablPtrWrapper' is a 'let'.  We are verifying that
  // the subscript has a nonmutating setter.
  let ablPtrWrapper = UnsafeMutableAudioBufferListPointer(ablPtr)

  if true {
    // Test getter.
    let audioBuffer = AudioBuffer(
      mNumberChannels: 2, mDataByteSize: 1024,
      mData: UnsafeMutablePointer<Void>(bitPattern: 0x1234_5678))

    UnsafeMutablePointer<AudioBuffer>(
      UnsafeMutablePointer<UInt8>(ablPtr) + ablHeaderSize).memory = audioBuffer
    ablPtrWrapper.count = 1

    expectEqual(2, ablPtrWrapper[0].mNumberChannels)
    expectEqual(1024, ablPtrWrapper[0].mDataByteSize)
    expectEqual(audioBuffer.mData, ablPtrWrapper[0].mData)
  }

  if true {
    // Test setter.
    let audioBuffer = AudioBuffer(
      mNumberChannels: 5, mDataByteSize: 256,
      mData: UnsafeMutablePointer<Void>(bitPattern: 0x8765_4321 as UInt))

    ablPtrWrapper.count = 2
    ablPtrWrapper[1] = audioBuffer

    let audioBufferPtr = UnsafeMutablePointer<AudioBuffer>(
      UnsafeMutablePointer<UInt8>(ablPtr) + ablHeaderSize) + 1
    expectEqual(5, audioBufferPtr.memory.mNumberChannels)
    expectEqual(256, audioBufferPtr.memory.mDataByteSize)
    expectEqual(audioBuffer.mData, audioBufferPtr.memory.mData)
  }

  ablPtr.dealloc(sizeInBytes)
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
      mData: UnsafeMutablePointer<Void>(bitPattern: 0x1234_5678 + i * 10))

    ablPtrWrapper[i] = audioBuffer
    expected.append(audioBuffer)
  }

  // FIXME: use checkMutableRandomAccessCollection, when we have that function.
  checkRandomAccessCollection(
    expected, ablPtrWrapper, SourceLocStack().withCurrentLoc())
  free(ablPtrWrapper.unsafeMutablePointer)
}

runAllTests()

