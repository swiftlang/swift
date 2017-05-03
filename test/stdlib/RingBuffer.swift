// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let RingBufferTests = TestSuite("RingBuffer")
RingBuffer.test("basic") {
  expectEqual(RingBuffer<Int>(capacity: 5).capacity, 5)

  expectEqualSequence([1,2,3,4,5] as RingBuffer, make([1,2,3,4,5]))
}

RingBuffer.test("append") {
  expectEqualSequence(make([]) { $0.append(1) },
    make([1]))

  expectEqualSequence(make([0,1,2]) { $0.append(contentsOf: [3,4]) },

  expectEqualSequence(make([0,1,2]) { $0.append(contentsOf: [3,4,5]) },
    make([1,2,3,4,5]))
}

RingBuffer.test("remove") {  
  expectEqualSequence(make([0,1,2]) { $0.removeSubrange(1..<1) },
    make([0,1,2]))

  expectEqualSequence(make([0,1,2,3]) { $0.removeSubrange(0...2) },
    make([3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.removeSubrange(0...2) },
    make([3,4]))

  expectEqualSequence(make([0,1,2,3]) { $0.removeSubrange(1...2) },
    make([0,3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.removeSubrange(1...2) },
    make([0,3,4]))

  expectEqualSequence(make([0,1,2,3]) { $0.removeSubrange(2...3) },
    make([0,1]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.removeSubrange(2...4) },
    make([0,1]))

  expectEqualSequence(make([0,1,2,3]) { $0.removeSubrange(0...3) },
    make([]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.removeSubrange(0...4) },
    make([]))

  expectEqualSequence(make([0,1,2,3]) { $0.removeAll() },
    make([]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.removeAll() },
    make([]))
}

RingBuffer.test("rotate") {
  expectEqualSequence(make([0,1,2,3]) { $0.rotate(shiftingToStart: 0) },
    make([0,1,2,3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.rotate(shiftingToStart: 0) },
    make([0,1,2,3,4]))

  expectEqualSequence(make([0,1,2,3]) { $0.rotate(shiftingToStart: 2) },
    make([2,3,0,1]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.rotate(shiftingToStart: 2) },
    make([2,3,4,0,1]))
}

RingBuffer.test("replaceSubrange") {
  expectEqualSequence(make([]) { $0.replaceSubrange(0..<0, with: [0,1]) },
    make([0,1]))

  expectEqualSequence(make([0,1,2,3]) { $0.replaceSubrange(1...2, with: []) },
    make([0,3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.replaceSubrange(1...2,
      with: []) },
    make([0,3,4]))

  expectEqualSequence(make([0,1,2,3]) { $0.replaceSubrange(1...2,
      with: [8,9]) },
    make([0,8,9,3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.replaceSubrange(1...2,
      with: [8,9]) },
    make([0,8,9,3,4]))

  expectEqualSequence(make([0,1,2,3]) { $0.replaceSubrange(1...2, with: [9]) },
    make([0,9,3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.replaceSubrange(1...2,
      with: [9]) },
    make([0,9,3,4]))

  expectEqualSequence(make([0,1,2,3]) { $0.replaceSubrange(1...2,
      with: [7,8,9]) },
    make([0,7,8,9,3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.replaceSubrange(1...2,
      with: [7,8,9]) },
    make([0,8,9,3,4]))

  expectEqualSequence(make([0,1,2,3]) { $0.replaceSubrange(1...2,
      with: [5,6,7,8,9]) },
    make([0,7,8,9,3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0.replaceSubrange(1...2,
      with: [5,6,7,8,9]) },
    make([0,8,9,3,4]))
}

RingBuffer.test("subrange") {
  expectEqualSequence(make([0,1,2,3])[1...2], [1,2])
  expectEqualSequence(make([0,1],[2,3,4])[1...3], [1,2,3])
  expectEqualSequence(make([0,1],[2,3,4])[3...3], [3])

  expectEqualSequence(make([0,1,2,3]) { $0[1...2].removeAll() },
    make([0,3]))
  expectEqualSequence(make([0,1],[2,3,4]) { $0[1...3].removeAll() },
    make([0,4]))
}

RingBuffer.test("description") {
  expectEqualSequence(make([0,1,2,3]).description,
    "[0, 1, 2, 3]")
  expectEqualSequence(make([0,1],[2,3,4]).description,
    "[0, 1, 2, 3, 4]")

  expectEqualSequence(make([0,1,2,3]).debugDescription,
    "RingBuffer<Int,5>([0, 1, 2, 3][])")
  expectEqualSequence(make([0,1],[2,3,4]).debugDescription,
    "RingBuffer<Int,5>([0, 1][2, 3, 4])")
}

runAllTests()

func make(_ prefix: [Int],
  _ suffix: [Int] = [],
  apply: (inout RingBuffer<Int>)->Void = { _ in }) -> RingBuffer<Int> {
  precondition(suffix.count == 0 || (suffix.count + prefix.count) == 5)
  var buffer = RingBuffer(ContiguousArray(suffix + prefix),
    capacity: 5,
    offset: suffix.count)
  apply(&buffer)
  return buffer
}

