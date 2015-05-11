// RUN: %target-parse-verify-swift

protocol Eq {
  func ==(lhs: Self, rhs: Self) -> Bool
  func !=(lhs: Self, rhs: Self) -> Bool
}

protocol Comparable: Eq {
  func <(lhs: Self, rhs: Self) -> Bool
  func <=(lhs: Self, rhs: Self) -> Bool
  func >=(lhs: Self, rhs: Self) -> Bool
  func >(lhs: Self, rhs: Self) -> Bool
}

func find<R : GeneratorType where R.Element : Eq>
       (range : R, value : R.Element) -> R {
  var result = range
//  var z = GeneratorSequence(range)
  for x in GeneratorSequence(range) {
    if x == value {
      break
    }
    result.next()
  }
  return result
}

func findIf<R : GeneratorType>(range: R, predicate: (R.Element) -> Bool) -> R {
  var result = range
  for x in GeneratorSequence(range) {
    if predicate(x) {
      break
    }
    result.next()
  }
  return result
}

func count<R : GeneratorType where R.Element : Eq>
       (range : R, value : R.Element) -> Int {
  var result = 0
  for x in GeneratorSequence(range) {
    if x == value {
      ++result
    }
  }
  return result
}

func countIf<
  R : GeneratorType
>(range: R, predicate: (R.Element) -> Bool) -> Int {
  var result = 0
  for x in GeneratorSequence(range) {
    if predicate(x) {
      ++result
    }
  }
  return result
}

func equal<R1 : GeneratorType, R2 : GeneratorType where R1.Element : Eq,
                                           R1.Element == R2.Element>
       (var range1 : R1, var range2 : R2) -> Bool {

  var e1 = range1.next()
  var e2 = range2.next()
    
  while (e1 != nil) && (e2 != nil) {
    if e1! != e2! {
      return false
    }
    e1 = range1.next()
    e2 = range2.next()
  }
  return (e1 == nil) == (e2 == nil)
}

func equalIf<R1 : GeneratorType, R2 : GeneratorType>
       (var range1 : R1, var range2 : R2,
        predicate : (R1.Element, R2.Element)-> Bool) -> Bool {
  var e1 = range1.next()
  var e2 = range2.next()
    
  while (e1 != nil) && (e2 != nil) {
    if !predicate(e1!, e2!) {
      return false
    }
    e1 = range1.next()
    e2 = range2.next()
  }
  return (e1 == nil) == (e2 == nil)
}

func mismatch<R1 : GeneratorType, R2 : GeneratorType where R1.Element : Eq,
                                              R1.Element == R2.Element>
       (var range1 : R1, var range2 : R2) -> (R1, R2) {
  var prev1 = range1, prev2 = range2

  while true {
    let e1 = range1.next(), e2 = range2.next()
    
    if (e1 == nil) || (e2 == nil) || e1! != e2! { break }
    prev1.next()
    prev2.next()
  }

  return (prev1, prev2)
}

func mismatchIf<R1 : GeneratorType, R2 : GeneratorType>
       (var range1 : R1, var range2 : R2,
        predicate : (R1.Element, R2.Element) -> Bool) -> (R1, R2) {
  var prev1 = range1, prev2 = range2

  while true {
    let e1 = range1.next(), e2 = range2.next()
    
    if (e1 == nil) || (e2 == nil) || !predicate(e1!, e2!) { break }
    prev1.next()
    prev2.next()
  }

  return (prev1, prev2)
}

func minElement<R : GeneratorType where R.Element : Comparable>(var range: R)
       -> R.Element {
  var result = range.next()!
  for next in GeneratorSequence(range) {
    if next < result {
      result = next
    }
  }
  return result
}

func maxElement<R : GeneratorType where R.Element : Comparable>(var range: R)
       -> R.Element {
  var result = range.next()!
  for next in GeneratorSequence(range) {
    if next > result {
      result = next
    }
  }
  return result
}

func minMaxElement<R : GeneratorType where R.Element : Comparable>(var range: R)
       -> (R.Element, R.Element) {
  var min = range.next()!, max = min
  for next in GeneratorSequence(range) {
    if next < min { min = next }
    if max < next { max = next }
  }
  return (min, max)
}

protocol RandomAccessStreamType : GeneratorType {
  func size() -> Int
  func getNth(n: Int) -> Element
  subscript (r : Range<Int>) -> Self { get }
}

func lowerBound<R : RandomAccessStreamType where R.Element : Comparable>
       (inputrange : R, value : R.Element) -> R {
  var range = inputrange
  while range.size() > 1 {
    let mid = range.size() / 2
    if range.getNth(mid) < value {
      range = range[mid + 1..<range.size()]
    } else {
      range = range[0..<mid]
    }
  }
  return range
}

func upperBound<R : RandomAccessStreamType where R.Element : Comparable>
       (inputrange : R, value : R.Element) -> R {
  var range = inputrange
  while range.size() > 1 {
    let mid = range.size() / 2
    if value < range.getNth(mid) {
      range = range[0..<mid]
    } else {
      range = range[mid + 1..<range.size()]
    }
  }
  return range
}
