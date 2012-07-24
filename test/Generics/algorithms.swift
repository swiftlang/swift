// RUN: %swift %s -parse -verify

// FIXME: Not working yet...
protocol Comparable {
  func [infix=160] ==(lhs : This, rhs : This) -> Bool
  func [infix=160] !=(lhs : This, rhs : This) -> Bool
}

protocol MethodComparable {
  func isEqual(other : This) -> Bool
}

func find<R : Range requires R.Element : MethodComparable>
       (range : R, value : R.Element) -> R {
  while !range.isEmpty() {
    var previous = range
    if range.getFirstAndAdvance().isEqual(value) {
      return previous
    }
  }
  return range
}

func findIf<R : Range>(range : R, pred : (R.Element) -> Bool) -> R {
  while !range.isEmpty() {
    var previous = range
    if pred(range.getFirstAndAdvance()) {
      return previous
    }
  }
  return range
}

func count<R : Range requires R.Element : MethodComparable>
       (range : R, value : R.Element) -> Int {
  var result = 0
  while !range.isEmpty() {
    if range.getFirstAndAdvance().isEqual(value) {
      ++result
    }
  }
  return result
}

func countIf<R : Range>(range : R, pred : (R.Element) -> Bool) -> Int {
  var result = 0
  while !range.isEmpty() {
    if pred(range.getFirstAndAdvance()) {
      ++result
    }
  }
  return result
}

func equal<R1 : Range, R2 : Range requires R1.Element : MethodComparable,
                                           R1.Element == R2.Element>
       (range1 : R1, range2 : R2) -> Bool {
  while !range1.isEmpty() && !range2.isEmpty() {
    if !range1.getFirstAndAdvance().isEqual(range2.getFirstAndAdvance()) {
      return false;
    }
  }

  return range1.isEmpty() && range2.isEmpty()
}

func equalIf<R1 : Range, R2 : Range> 
       (range1 : R1, range2 : R2, pred : (R1.Element, R2.Element)-> Bool) 
         -> Bool {
  while !range1.isEmpty() && !range2.isEmpty() {
    if !pred(range1.getFirstAndAdvance(), range2.getFirstAndAdvance()) {
      return false;
    }
  }

  return range1.isEmpty() && range2.isEmpty()
}

func mismatch<R1 : Range, R2 : Range requires R1.Element : MethodComparable,
                                              R1.Element == R2.Element>
       (range1 : R1, range2 : R2) -> (R1, R2) {
  while !range1.isEmpty() && !range2.isEmpty() {
     var (prev1, prev2) = (range1, range2)
     if !range1.getFirstAndAdvance().isEqual(range2.getFirstAndAdvance()) {
      return (prev1, prev2)
    }
  }

  return (range1, range2)
}

func mismatchIf<R1 : Range, R2 : Range>
       (range1 : R1, range2 : R2, pred : (R1.Element, R2.Element) -> Bool) 
         -> (R1, R2) {
  while !range1.isEmpty() && !range2.isEmpty() {
     var (prev1, prev2) = (range1, range2)
     if !pred(range1.getFirstAndAdvance(), range2.getFirstAndAdvance()) {
      return (prev1, prev2)
    }
  }

  return (range1, range2)
}

