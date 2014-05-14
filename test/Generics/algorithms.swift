// RUN: %swift %s -parse -verify

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

func find<R : Generator where R.Element : Eq>
       (range : R, value : R.Element) -> R {
  var result = range
  var z = GeneratorSequence(range)
  for x in GeneratorSequence(range) {
    if x == value {
      break
    }
    result.next()
  }
  return result
}

func findIf<R : Generator>(range: R, pred: (R.Element) -> Bool) -> R {
  var result = range
  for x in GeneratorSequence(range) {
    if pred(x) {
      break
    }
    result.next()
  }
  return result
}

func count<R : Generator where R.Element : Eq>
       (range : R, value : R.Element) -> Int {
  var result = 0
  for x in GeneratorSequence(range) {
    if x == value {
      ++result
    }
  }
  return result
}

func countIf<R : Generator>(range: R, pred: (R.Element) -> Bool) -> Int {
  var result = 0
  for x in GeneratorSequence(range) {
    if pred(x) {
      ++result
    }
  }
  return result
}

func equal<R1 : Generator, R2 : Generator where R1.Element : Eq,
                                           R1.Element == R2.Element>
       (var range1 : R1, var range2 : R2) -> Bool {

  var e1 = range1.next()
  var e2 = range2.next()
    
  while e1 && e2 {
    if e1! != e2! {
      return false
    }
    e1 = range1.next()
    e2 = range2.next()
  }
  return !e1 == !e2
}

func equalIf<R1 : Generator, R2 : Generator>
       (var range1 : R1, var range2 : R2,
        pred : (R1.Element, R2.Element)-> Bool) -> Bool {
  var e1 = range1.next()
  var e2 = range2.next()
    
  while e1 && e2 {
    if !pred(e1!, e2!) {
      return false
    }
    e1 = range1.next()
    e2 = range2.next()
  }
  return !e1 == !e2
}

func mismatch<R1 : Generator, R2 : Generator where R1.Element : Eq,
                                              R1.Element == R2.Element>
       (var range1 : R1, var range2 : R2) -> (R1, R2) {
  var prev1 = range1, prev2 = range2

  while true {
    var e1 = range1.next(), e2 = range2.next()
    
    if !e1 || !e2 || e1! != e2! { break }
    prev1.next()
    prev2.next()
  }

  return (prev1, prev2)
}

func mismatchIf<R1 : Generator, R2 : Generator>
       (var range1 : R1, var range2 : R2,
        pred : (R1.Element, R2.Element) -> Bool) -> (R1, R2) {
  var prev1 = range1, prev2 = range2

  while true {
    var e1 = range1.next(), e2 = range2.next()
    
    if !e1 || !e2 || !pred(e1!, e2!) { break }
    prev1.next()
    prev2.next()
  }

  return (prev1, prev2)
}

func minElement<R : Generator where R.Element : Comparable>(var range: R)
       -> R.Element {
  var result = range.next()!
  for next in GeneratorSequence(range) {
    if next < result {
      result = next
    }
  }
  return result
}

func maxElement<R : Generator where R.Element : Comparable>(var range: R)
       -> R.Element {
  var result = range.next()!
  for next in GeneratorSequence(range) {
    if next > result {
      result = next
    }
  }
  return result
}

func minMaxElement<R : Generator where R.Element : Comparable>(var range: R)
       -> (R.Element, R.Element) {
  var min = range.next()!, max = min
  for next in GeneratorSequence(range) {
    if next < min { min = next }
    if max < next { max = next }
  }
  return (min, max)
}

protocol RandomAccessStreamType : Generator {
  func size() -> Int
  func getNth(n: Int) -> Element
  subscript (r : Range<Int>) -> Self { get }
}

func lowerBound<R : RandomAccessStreamType where R.Element : Comparable>
       (inputrange : R, value : R.Element) -> R {
  var range = inputrange
  while range.size() > 1 {
    var mid = range.size() / 2
    if range.getNth(mid) < value {
      range = range[mid + 1..range.size()]
    } else {
      range = range[0..mid]
    }
  }
  return range
}

func upperBound<R : RandomAccessStreamType where R.Element : Comparable>
       (inputrange : R, value : R.Element) -> R {
  var range = inputrange
  while range.size() > 1 {
    var mid = range.size() / 2
    if value < range.getNth(mid) {
      range = range[0..mid]
    } else {
      range = range[mid + 1..range.size()]
    }
  }
  return range
}
