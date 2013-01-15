// FIXME: make generic
// BLOCKED: <rdar://problem/12780068> Crash while trying to make iteration generic
struct IntEnumeratorType : Enumerator, Enumerable {
  typealias Element = Int
  var min : Int
  var max : Int
  var stride : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func by(s : Int) -> IntEnumeratorType {
    var result = this
    result.stride = s
    return result
  }

  func isEmpty() -> Bool {
    return min >= max
  }
  func contains(x : Int) -> Bool {
    return min <= x && x < max
  }

  func next() -> Int {
    var prev = min
    min += stride
    return prev
  }

  typealias EnumeratorType = IntEnumeratorType
  func getEnumeratorType() -> IntEnumeratorType {
    return this
  }

  func replPrint() {
    print("\(min)..\(max)")
    if stride != 1 {
      print(" by \(stride)")
    }
  }
}

// FIXME: make generic
// BLOCKED: <rdar://problem/12780068> Crash while trying to make iteration generic
struct ReverseIntEnumeratorType : Enumerator, Enumerable {
  typealias Element = Int
  var min : Int
  var max : Int
  var stride : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func isEmpty() -> Bool {
    return min >= max
  }
  func contains(x : Int) -> Bool {
    return min <= x && x < max
  }
  func next() -> Int {
    max -= stride
    return max
  }

  typealias EnumeratorType = ReverseIntEnumeratorType
  func getEnumeratorType() -> ReverseIntEnumeratorType {
    return this
  }

  func replPrint() {
    print("reverse(\(min)..\(max))")
    if stride != 1 {
      print(" by \(stride)")
    }
  }
}

func reverse(rng : IntEnumeratorType) -> ReverseIntEnumeratorType {
  return ReverseIntEnumeratorType(rng.min, rng.max, rng.stride)
}

func reverse(rng : ReverseIntEnumeratorType) -> IntEnumeratorType {
  return IntEnumeratorType(rng.min, rng.max, rng.stride)
}

func [infix_left=110] .. (min : Int, max : Int) -> IntEnumeratorType {
  return IntEnumeratorType(min, max, 1)
}

// FIXME: make generic
// BLOCKED: <rdar://problem/12780068> Crash while trying to make iteration generic
struct DoubleEnumeratorType : Enumerator, Enumerable {
  typealias Element = Double
  var min : Double,
  max : Double,
  stride : Double

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Double) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Double, f : (Double, Double) -> Double) -> Double {
    for i in this { val = f(val, i) }
    return val
  }

  func by(s : Double) -> DoubleEnumeratorType {
    var result = this
    result.stride = s
    return result
  }

  func isEmpty() -> Bool {
    return min >= max
  }
  func next() -> Double {
    var prev = min
    min += stride
    return prev
  }

  typealias EnumeratorType = DoubleEnumeratorType
  func getEnumeratorType() -> DoubleEnumeratorType {
    return this
  }

  func replPrint() {
    print("\(min)..\(max)")
    if stride != 1.0 {
      print(" by \(stride)")
    }
  }
}

func [infix_left=110] .. (min : Double, max : Double) -> DoubleEnumeratorType {
  return DoubleEnumeratorType(min, max, 1.0)
}
