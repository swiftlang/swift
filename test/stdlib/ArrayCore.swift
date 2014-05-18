//===--- ArrayCore.swift --------------------------------------------------===//
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
// RUN: %target-run-simple-swift

//===--- class Tracked ----------------------------------------------------===//
// Instead of testing with Int elements, we use this wrapper class
// that can help us track allocations and find issues with object
// lifetime inside Array implementations.
var trackedCount = 0
var nextTrackedSerialNumber = 0

class Tracked : ForwardIndex, Printable {
  init(_ value: Int) {
    ++trackedCount
    serialNumber = ++nextTrackedSerialNumber
    self.value = value
  }
  
  deinit {
    assert(serialNumber > 0, "double destruction!")
    --trackedCount
    serialNumber = -serialNumber
  }

  var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return value.description
  }

  func succ() -> Tracked {
    return Tracked(self.value.succ())
  }

  var value: Int
  var serialNumber: Int
}

func == (x: Tracked, y: Tracked) -> Bool {
  return x.value == y.value
}

//===--- struct MrMcRange -------------------------------------------------===//
// A wrapper around Range<Tracked> that allows us to detect when it is
// being treated as a Collection rather than merely a Sequence, which
// helps us to prove that an optimization is being used.  In
// particular, when constructing a ContiguousArrayBuffer from a
// Collection, the necessary storage should be pre-allocated.
struct MrMcRange : Collection {
  typealias Base = Range<Int>

  init(_ base: Base) {
    self.base = base
  }

  func generate() -> IndexingGenerator<MrMcRange> {
    return IndexingGenerator(self)
  }
  
  var startIndex: Int {
    println("using collection API")
    return base.startIndex
  }
  
  var endIndex: Int {
    return base.endIndex
  }

  subscript(i: Int) -> Tracked {
    return Tracked(base[i])
  }
  
  var base: Base
}

//===--- struct MrMcArray<T> ----------------------------------------------===//
// A faux ArrayType that allows us to detect that, rather than being
// treated as an arbitrary Collection when converting to a
// ContiguousArrayBuffer, it is first asked for its underlying
// ContiguousArrayBuffer.
struct MrMcArray<T> : Collection, _ArrayType {
  typealias _Buffer = ContiguousArrayBuffer<T>

  init(_ buffer: _Buffer) {
    self._buffer = buffer
  }
  
  var count: Int {
    return _buffer.count
  }

  typealias GeneratorType = IndexingGenerator<MrMcArray>
  func generate() -> GeneratorType {
    return IndexingGenerator(self)
  }
  
  var startIndex: Int {
    return 0
  }
  
  var endIndex: Int {
    return _buffer.count
  }

  subscript(i: Int) -> T {
    return _buffer[i]
  }

  var _buffer: _Buffer
}

func printSequence<T: Sequence>(x: T) {
  print("<")
  var prefix = ""
  for a in x {
    print(prefix)
    print(a)
    prefix = " "
  }
  println(">")
}



// CHECK: testing...
println("testing...")

func test() {
  //===--- Sequences can be converted -------------------------------------===//

  let n0 = ((Tracked(10)..Tracked(27)).generate())~>_copyToNativeArrayBuffer()
  // CHECK-NEXT: <10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26>
  printSequence(n0)

  //===--- Collections get measured ---------------------------------------===//

  // CHECK-NEXT: using collection API
  let n1 = MrMcRange(3..23)~>_copyToNativeArrayBuffer()
  // CHECK-NEXT: <3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22>
  printSequence(n1)

  //===--- _ArrayTypes get asked for a NativeBuffer -----------------------===//

  let a0 = MrMcArray<Tracked>(n1)

  let n2 = a0~>_copyToNativeArrayBuffer()
  // CHECK-NEXT: true
  println(n1 === n2)
}

// CHECK-NEXT: trackedCount = 0
println("trackedCount = \(trackedCount)")

// CHECK-NEXT: done.
println("done.")
