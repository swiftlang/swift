//===----------------------------------------------------------------------===//
// Range protocol and types
//===----------------------------------------------------------------------===//

/// \brief Describes iteration over a sequences of elements that can be 
/// performed once.
protocol Range {
  typealias Element
  func isEmpty() -> Bool
  func getFirstAndAdvance() -> Element
}

protocol Enumerable {
  typealias Elements : Range
  func getElements() -> Elements
}

struct IntRange : Range, Enumerable {
  typealias Element = Int
  var min : Int,
  max : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func isEmpty() -> Bool { return min >= max }
  func contains(x : Int) -> Bool {return min <= x && x < max}
  func contains(x : StringByte) -> Bool {return min <= Int(x) && Int(x) < max}

  func getFirstAndAdvance() -> Int {
    var prev = min
    ++min
    return prev
  }

  typealias Elements = IntRange
  func getElements() -> IntRange { return this }

  func replPrint() {
    print(min)
    print("..")
    print(max)
  }
}

struct ReverseIntRange : Range, Enumerable {
  typealias Element = Int

  var min : Int,
  max : Int

  // FIXME: each/reduce should be moved out to generic methods, or methods that
  // take the range as a protocol'd "enumeration/iterator" value.
  func each(f : (Int) -> Void) {
    for i in this { f(i) }
  }

  func reduce(val : Int, f : (Int, Int) -> Int) -> Int {
    for i in this { val = f(val, i) }
    return val
  }

  func isEmpty() -> Bool { return min >= max }
  func contains(x : Int) -> Bool {return min <= x && x < max}
  func getFirstAndAdvance() -> Int { 
    --max
    return max
  }

  typealias Elements = ReverseIntRange
  func getElements() -> ReverseIntRange { return this }

  func replPrint() {
    print("reverse(")
    print(min)
    print("..")
    print(max)
    print(')')
  }
}

func reverse(rng : IntRange) -> ReverseIntRange {
  return ReverseIntRange(rng.min, rng.max)
}

func reverse(rng : ReverseIntRange) -> IntRange {
  return IntRange(rng.min, rng.max)
}

func [infix_left=110] .. (min : Int, max : Int) -> IntRange {
  return IntRange(min, max)
}

struct DoubleRange : Range, Enumerable {
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
  
  // by - To be used as (0.0 .. 10.0).by(.25)
  func by(s : Double) -> DoubleRange {
    var result = this
    result.stride = s
    return result
  }

  func isEmpty() -> Bool { return min >= max }
  func getFirstAndAdvance() -> Double { 
    var prev = min
    min = min + stride
    return prev
  }

  typealias Elements = DoubleRange
  func getElements() -> DoubleRange { return this }

  func replPrint() {
    print(min)
    print("..")
    print(max)

    if stride != 1.0 {
      print(" by ")
      print(stride)
    }
  }

}

func [infix_left=110] .. (min : Double, max : Double) -> DoubleRange {
  return DoubleRange(min, max, 1.0)
}
