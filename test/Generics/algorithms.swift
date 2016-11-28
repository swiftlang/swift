// RUN: %target-typecheck-verify-swift

protocol Eq {
  static func ==(lhs: Self, rhs: Self) -> Bool
  static func !=(lhs: Self, rhs: Self) -> Bool
}

protocol Comparable: Eq {
  static func <(lhs: Self, rhs: Self) -> Bool
  static func <=(lhs: Self, rhs: Self) -> Bool
  static func >=(lhs: Self, rhs: Self) -> Bool
  static func >(lhs: Self, rhs: Self) -> Bool
}

func find<R : IteratorProtocol>(_ range : R, value : R.Element) -> R
  where R.Element : Eq {
  var result = range
  for x in IteratorSequence(range) {
    if x == value {
      break
    }
    _ = result.next()
  }
  return result
}

func findIf<R : IteratorProtocol>(
  _ range: R, predicate: (R.Element) -> Bool
) -> R {
  var result = range
  for x in IteratorSequence(range) {
    if predicate(x) {
      break
    }
    _ = result.next()
  }
  return result
}

func count<R : IteratorProtocol>(_ range : R, value : R.Element) -> Int
  where R.Element : Eq {
  var result = 0
  for x in IteratorSequence(range) {
    if x == value {
      result += 1
    }
  }
  return result
}

func countIf<
  R : IteratorProtocol
>(_ range: R, predicate: (R.Element) -> Bool) -> Int {
  var result = 0
  for x in IteratorSequence(range) {
    if predicate(x) {
      result += 1
    }
  }
  return result
}

func equal<
  R1 : IteratorProtocol,
  R2 : IteratorProtocol
  >(_ range1 : R1, range2 : R2) -> Bool
  where R1.Element : Eq, R1.Element == R2.Element {

  var range1 = range1
  var range2 = range2

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

func equalIf<R1 : IteratorProtocol, R2 : IteratorProtocol>
       (_ range1 : R1, range2 : R2,
        predicate : (R1.Element, R2.Element)-> Bool) -> Bool {
  var range1 = range1
  var range2 = range2
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

func mismatch<
  R1 : IteratorProtocol,
  R2 : IteratorProtocol
>(_ range1 : R1, range2 : R2) -> (R1, R2)
  where R1.Element : Eq, R1.Element == R2.Element {
  var range1 = range1
  var range2 = range2
  var prev1 = range1, prev2 = range2

  while true {
    let e1 = range1.next(), e2 = range2.next()
    
    if (e1 == nil) || (e2 == nil) || e1! != e2! { break }
    _ = prev1.next()
    _ = prev2.next()
  }

  return (prev1, prev2)
}

func mismatchIf<R1 : IteratorProtocol, R2 : IteratorProtocol>
       (_ range1 : R1, range2 : R2,
        predicate : (R1.Element, R2.Element) -> Bool) -> (R1, R2) {
  var range1 = range1
  var range2 = range2
  var prev1 = range1, prev2 = range2

  while true {
    let e1 = range1.next(), e2 = range2.next()
    
    if (e1 == nil) || (e2 == nil) || !predicate(e1!, e2!) { break }
    _ = prev1.next()
    _ = prev2.next()
  }

  return (prev1, prev2)
}

func minElement<R : IteratorProtocol>(_ range: R) -> R.Element
  where R.Element : Comparable {
  var range = range
  var result = range.next()!
  for next in IteratorSequence(range) {
    if next < result {
      result = next
    }
  }
  return result
}

func maxElement<R : IteratorProtocol>(_ range: R) -> R.Element
  where R.Element : Comparable {
  var range = range
  var result = range.next()!
  for next in IteratorSequence(range) {
    if next > result {
      result = next
    }
  }
  return result
}

func minMaxElement<R : IteratorProtocol>(_ range: R) -> (R.Element, R.Element)
  where R.Element : Comparable {
  var range = range
  var min = range.next()!, max = min
  for next in IteratorSequence(range) {
    if next < min { min = next }
    if max < next { max = next }
  }
  return (min, max)
}

protocol RandomAccessStream : IteratorProtocol {
  func size() -> Int
  func getNth(_ n: Int) -> Element
  subscript (r : Range<Int>) -> Self { get }
}

func lowerBound<R : RandomAccessStream>
  (_ inputrange : R, value : R.Element) -> R
  where R.Element : Comparable {
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

func upperBound<R : RandomAccessStream>
  (_ inputrange : R, value : R.Element) -> R
  where R.Element : Comparable {
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
