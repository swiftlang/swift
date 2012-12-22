//===----------------------------------------------------------------------===//
// Swift Algorithm Library.
//===----------------------------------------------------------------------===//
func minElement<R : Enumerator requires R.Element : Comparable >(range : R)
       -> R.Element {
  var result = range.next()
  while !range.isEmpty() {
    var next = range.next()
    if next < result { result = next }
  }
  return result
}

func maxElement<R : Enumerator requires R.Element : Comparable >(range : R)
       -> R.Element {
  var result = range.next()
  while !range.isEmpty() {
    var next = range.next()
    if next > result { result = next }
  }
  return result
}

func find<T : Equality>(array : T[], value : T) -> Int {
  var idx = 0
  for elt in array {
    if elt == value { return idx }
    ++idx
  }
  return -1
}

// FIXME: use generic predicated sort()
// BLOCKED: <rdar://problem/12780569> Crash in IRGen while making sort()/map() generic
func sort<T : Comparable>(array : T[]) -> T[] {
  var result = array.copy()
  for i in 0..result.length {
    for j in i+1..result.length {
      if result[j] < result[i] {
        // FIXME: Use swap()
        // BLOCKED: <rdar://problem/12782554> write-back properties do not work
        var temp = result[i]
        result[i] = result[j]
        result[j] = temp
      }
    }
  }
  return result
}

// FIXME: make generic
// BLOCKED: <rdar://problem/12780569> Crash in IRGen while making sort()/map() generic
func sort(array : String[], pred : (String, String) -> Bool) -> String[] {
  var result = array.copy()
  for i in 0..result.length {
    for j in i+1..result.length {
      if pred(result[j], result[i]) {
        // FIXME: Use swap()
        // BLOCKED: <rdar://problem/12782554> write-back properties do not work
        var temp = result[i]
        result[i] = result[j]
        result[j] = temp
      }
    }
  }
  return result
}

// FIXME: make generic
// BLOCKED: <rdar://problem/12780569> Crash in IRGen while making sort()/map() generic
func sort(array : DictionaryItem[],
          pred : (DictionaryItem, DictionaryItem) -> Bool)
       -> DictionaryItem[] {
  var result = array.copy()
  for i in 0..result.length {
    for j in i+1..result.length {
      if pred(result[j], result[i]) {
        // FIXME: Use swap()
        // BLOCKED: <rdar://problem/12782554> write-back properties do not work
        var temp = result[i]
        result[i] = result[j]
        result[j] = temp
      }
    }
  }
  return result
}

func map<T>(array : T[], fn : (T) -> T) -> T[] {
  var result = new T[array.length]
  for i in 0..array.length {
    result[i] = fn(array[i])
  }
  return result
}

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

typealias Any = protocol<>

protocol Equality {
  func [infix=160] ==(lhs : This, rhs : This) -> Bool
  func [infix=160] !=(lhs : This, rhs : This) -> Bool
}

protocol Comparable : Equality {
  func [infix=170] <(lhs : This, rhs : This) -> Bool
  func [infix=170] <=(lhs : This, rhs : This) -> Bool
  func [infix=170] >=(lhs : This, rhs : This) -> Bool
  func [infix=170] >(lhs : This, rhs : This) -> Bool
}

protocol Hashable : Equality {
  func hash() -> UInt
}

func swap<T>(a : [byref] T, b : [byref] T) {
  var c = a
  a = b
  b = c
}

// FIXME -- BLOCKED: <rdar://problem/12695123> Crash while trying to make min/max generic
//func min<T : Comparable>(x : T, y : T, rest : T...) -> T {
func min(x : Int, y : Int, rest : Int...) -> Int {
  var r = x
  if y < x {
    r = y
  }
  for z in rest {
    if z < r {
      r = z
    }
  }
  return r
}

// FIXME -- BLOCKED: <rdar://problem/12695123> Crash while trying to make min/max generic
//func max<T : Comparable>(x : T, y : T, rest : T...) -> T {
func max(x : Int, y : Int, rest : Int...) -> Int {
  var r = y
  if y < x {
    r = x
  }
  for z in rest {
    if z >= r {
      r = z
    }
  }
  return r
}
