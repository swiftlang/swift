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

struct MrMcRange : Collection {
  typealias Base = Range<Int>

  func generate() -> Base.GeneratorType {
    return base.generate()
  }
  
  var startIndex: Int {
    println("using collection API")
    return base.startIndex
  }
  
  var endIndex: Int {
    return base.endIndex
  }

  subscript(i: Int) -> Int {
    return base[i]
  }
  
  var base: Base
}

struct MrMcArray<T> : Collection, _ArrayType {
  typealias Base = NativeArrayBuffer<GeneratorType.Element>

  init(base: Base) {
    self.base = base
  }
  
  var count: Int {
    return base.count
  }

  typealias GeneratorType = IndexingGenerator<MrMcArray>
  func generate() -> GeneratorType {
    return IndexingGenerator(self)
  }
  
  var startIndex: Int {
    return 0
  }
  
  var endIndex: Int {
    return base.count
  }

  subscript(i: Int) -> Int {
    return base[i]
  }

  func _asNativeBuffer() -> Base {
    return base
  }

  var base: Base
}

func printBuffer<T: ReplPrintable>(buf: NativeArrayBuffer<T>) {
  print("<")
  for i in 0...buf.count {
    if i != 0 {
      print(" ")
    }
    buf[i].replPrint()
  }
  println(">")
}


// CHECK: testing...
println("testing...")

//===--- Sequences can be converted ---------------------------------------===//

let n0 = asNativeArrayBuffer((10...27).generate(), false)
// CHECK-NEXT: <10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26>
printBuffer(n0)

//===--- Collections get measured -----------------------------------------===//

// CHECK-NEXT: using collection API
let n1 = asNativeArrayBuffer(MrMcRange(3...23), false)
// CHECK-NEXT: <3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22>
printBuffer(n1)

//===--- _ArrayTypes get asked for a NativeBuffer -------------------------===//

let a0 = MrMcArray<Int>(n1)

// If we ask for a COW-compatible buffer, it is provided
let n2 = asNativeArrayBuffer(a0, false)
// CHECK-NEXT: true
println(n1 === n2)

// Otherwise, we get a copy
let n3 = asNativeArrayBuffer(a0, true)
// CHECK-NEXT: false
println(n1 === n3)
// CHECK-NEXT: <3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22>
printBuffer(n3)

// CHECK-NEXT: done.
println("done.")
