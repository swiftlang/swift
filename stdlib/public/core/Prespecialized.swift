//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Pre-specialization of some popular generic classes and functions.
//===----------------------------------------------------------------------===//

struct _Prespecialize {
  // Create specializations for the arrays of most
  // popular builtin integer and floating point types.
  static internal func _specializeArrays() {
    func _createArrayUser<Element : Comparable>(sampleValue: Element) {
      // Initializers.
      let _: [Element] = [ sampleValue ]
      var a = [Element](count: 1, repeatedValue: sampleValue)

      // Read array element
      let _ =  a[0]

      // Set array elements
      for j in 0..<a.count {
        a[0] = a[j]
      }

      for i1 in 0..<a.count {
        for i2 in 0..<a.count {
          a[i1] = a[i2]
        }
      }

      a[0] = sampleValue

      // Get length and capacity
      let _ = a.count + a.capacity

      // Iterate over array
      for e in a {
        print(e)
        print("Value: \(e)")
      }

      print(a)

      // Reserve capacity
      a.removeAll()
      a.reserveCapacity(100)

      // Sort array
      let _ = a.sort { (a:Element, b:Element) in a < b }
      a.sortInPlace { (a:Element, b:Element) in a < b }


      // force specialization of print<Element>
      print(sampleValue)
      print("Element:\(sampleValue)")
    }

    _createArrayUser(1 as Int)
    _createArrayUser(1 as Int8)
    _createArrayUser(1 as Int16)
    _createArrayUser(1 as Int32)
    _createArrayUser(1 as Int64)
    _createArrayUser(1 as UInt)
    _createArrayUser(1 as UInt8)
    _createArrayUser(1 as UInt16)
    _createArrayUser(1 as UInt32)
    _createArrayUser(1 as UInt64)
    _createArrayUser("a" as String)
    _createArrayUser(1.5 as Float)
    _createArrayUser(1.5 as Double)
  }

  // Force pre-specialization of Range<Int>
  static internal func _specializeRanges() -> Int {
    let a = [Int](count: 10, repeatedValue: 1)
    var count = 0
    // Specialize Range for integers
    for i in 0..<a.count {
      count += a[i]
    }
    return count
  }
}
