//===----------------------------------------------------------------------===//
// Swift Algorithm Library.
// ===----------------------------------------------------------------------===//
func minElement<
     R : Enumerator 
       requires R.Element : Comparable>(range : R)
  -> R.Element {
  var result = range.next()
  while !range.isEmpty() {
    var next = range.next()
    if next < result { result = next }
  }
  return result
}

func maxElement<
     R : Enumerator 
       requires R.Element : Comparable>(range : R)
  -> R.Element {
  var result = range.next()
  while !range.isEmpty() {
    var next = range.next()
    if next > result { result = next }
  }
  return result
}

func find<T : Equatable>(array : T[], value : T) -> Int {
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
func sort(array : (key: String, value: Int)[],
          pred : ((key: String, value: Int), (key: String, value: Int)) -> Bool)
       -> (key: String, value: Int)[] {
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

// FIXME: map() should support returning an array of a different type:
// func map<U>(f : (T) -> U) -> U[] {
func map<T>(array : T[], fn : (T) -> T) -> T[] {
  var result = new T[array.length]
  for i in 0..array.length {
    result[i] = fn(array[i])
  }
  return result
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

func min(x : Double, y : Double, rest : Double...) -> Double {
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

func max(x : Double, y : Double, rest : Double...) -> Double {
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

func split<
  Seq: Sliceable, 
  IsSeparator: Predicate 
    requires IsSeparator.Arguments == Seq.Element
>(seq: Seq, 
  isSeparator: IsSeparator, 
  maxSplit: Int = Int.max(),
  allowEmptySlices: Bool = false
  ) -> Seq[] {

  var result = Vector<Seq>()

  var startIndex = allowEmptySlices ? Some(seq.begin()) : None
  var splits = 0

  for j in indices(seq) {
    if apply(isSeparator, seq.__getitem__(j)) {
      if startIndex {
        var i = startIndex.get()
        result.append(seq.__slice__(i, j))
        startIndex = Some(j.succ())
        if ++splits >= maxSplit {
          break
        }
        if !allowEmptySlices {
          startIndex = None
        }
      }
    }
    else {
      if !startIndex {
        startIndex = Some(j)
      }
    }
  }

  for i in startIndex { // destructuring bind
    result.append(seq.__slice__(i, seq.end()))
  }
  return result.takeArray()
}

// FIXME: Until <rdar://problem/13985164> is fixed, we can't build
// generic algorithms that work on Enumerables, so we're operating on
// Enumerators here.

/// \brief Return true iff the elements of e1 are equal to the initial
/// elements of e2

func startsWith<
  E0: Enumerator, E1: Enumerator
  requires 
    E0.Element == E1.Element, 
    E0.Element : Equatable
>(e0: E0, e1: E1) -> Bool
{
  while !e0.isEmpty() {
    if e1.isEmpty() { return true }
    if e0.next() != e1.next() {
      return false
    }
  }
  return e1.isEmpty()
}
