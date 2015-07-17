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
// Pre-specializaiton of some popular generic classes and functions.
//===----------------------------------------------------------------------===//

public struct _ArrayUser<T: Comparable> {
  @inline(never)
  public func _getArray(a: [T], _ i: Int) -> T {
    return a[i]
  }

  @inline(never)
  public func _setArray(inout a: [T], _ i: Int, _ v: T) {
    for j in 0..<a.count {
      a[i] = a[j]
    }

    for var i1 = 0; i1 < a.count; ++i1 {
     for var i2 = 0; i2 < a.count; ++i2 {
        a[i1] = a[i2]
     }
    }

    a[i] = v
  }

  @inline(never)
  public func _getLength(a : [T]) -> Int {
    return a.count + a.capacity
  }

  @inline(never)
  public func _iterateOverArray(a: [T]) {
    for e in a {
      print(e)
      print("Value: \(e)")
    }
    print(a)
  }
  
  @inline(never)
  public func _reserveCapacity(inout a: [T]) {
    a.removeAll()
    a.reserveCapacity(100)
  }
  
  @inline(never)
  public func _sort(inout a: [T]) -> [T] {
    let b = a.sort { (a:T, b:T) in a < b }
    return a.sort { (a:T, b:T) in a < b }
  }
}

public func _createArrayUser<T: Comparable>(_a: [T], _ e: T) {
  let au = _ArrayUser<T>()
  var a = _a
  au._getArray(a, 0)
  au._setArray(&a, 0, e)
  au._iterateOverArray(a)
  au._reserveCapacity(&a)
  // force specialization of print<T>
  print(e)
  print("Element:\(e)")
}

// Create specializations for the arrays of most 
// popular builtin types.

private class Obj {
}

public func _specializeArrays() {
  let arIntLiteral: [Int] = [1,2]
  let arInt: [Int] = [Int](count: 2, repeatedValue: 0)
  _createArrayUser(arInt, 1)

  let arInt8Literal: [Int8] = [1,2]
  let arInt8: [Int8] = [Int8](count: 2, repeatedValue: 0)
  _createArrayUser(arInt8, 1)


  let arInt16Literal: [Int16] = [1,2]
  let arInt16: [Int16] = [Int16](count: 2, repeatedValue: 0)
  _createArrayUser(arInt16, 1)

  let arInt32Literal: [Int32] = [1,2]
  let arInt32: [Int32] = [Int32](count: 2, repeatedValue: 0)
  _createArrayUser(arInt32, 1)

  let arInt64Literal: [Int64] = [1,2]
  let arInt64: [Int64] = [Int64](count: 2, repeatedValue: 0)
  _createArrayUser(arInt64, 1)

  let arUIntLiteral: [UInt] = [1,2]
  let arUInt: [UInt] = [UInt](count: 2, repeatedValue: 0)
  _createArrayUser(arUInt, 1)

  let arUInt8Literal: [UInt8] = [1,2]
  let arUInt8: [UInt8] = [UInt8](count: 2, repeatedValue: 0)
  _createArrayUser(arUInt8, 1)


  let arUInt16Literal: [UInt16] = [1,2]
  let arUInt16: [UInt16] = [UInt16](count: 2, repeatedValue: 0)
  _createArrayUser(arUInt16, 1)
  
  let arUInt32Literal: [UInt32] = [1,2]
  let arUInt32: [UInt32] = [UInt32](count: 2, repeatedValue: 0)
  _createArrayUser(arUInt32, 1)

  let arUInt64Literal: [UInt64] = [1,2]
  let arUInt64: [UInt64] = [UInt64](count: 2, repeatedValue: 0)
  _createArrayUser(arUInt64, 1)

  let arStringLiteral: [String] = ["1", "2"]
  let arString: [String] = [String](count: 2, repeatedValue: "")
  _createArrayUser(arString, "1")

  let arFloatLiteral: [Float] = [1.1, 2.2]
  let arFloat: [Float] = [Float](count: 2, repeatedValue: 1.1)
  _createArrayUser(arFloat, 1.1)

  let arDoubleLiteral: [Double] = [1.1, 2.2]
  let arDouble: [Double] = [Double](count: 2, repeatedValue: 1.1)
  _createArrayUser(arDouble, 1.1)

  /*
  let arAnyObjectLiteral: [AnyObject] = [Obj(), Obj()]
  let arAnyObject: [AnyObject] = [AnyObject](count: 2, repeatedValue: Obj())
  _createArrayUser(arAnyObject, Obj() as AnyObject)
  */
}

public func _specializeRanges() -> Int {
  let a = [Int](count: 10, repeatedValue: 1)
  var count = 0
  // Specialize Range for integers
  for i in 0..<a.count {
    count += a[i]
  }
  return count
}

public func _set_array_elem<T>(inout ar: [T], _ i: Int, _ elem: T) {
  ar[i] = elem
}
