// RUN: %target-parse-verify-swift -parse-stdlib

import Swift

infix operator < : ComparisonPrecedence
precedencegroup ComparisonPrecedence {
  associativity: none
  higherThan: EqualityPrecedence
}

infix operator == : EqualityPrecedence
infix operator != : EqualityPrecedence
precedencegroup EqualityPrecedence {
  associativity: none
  higherThan: AssignmentPrecedence
}

precedencegroup AssignmentPrecedence {
  assignment: true
}


func testslice(_ s: Array<Int>) {
  for i in 0..<s.count { print(s[i]+1) }
  for i in s { print(i+1) }
  _ = s[0..<2]
  _ = s[0...1]
}

@_silgen_name("malloc") func c_malloc(_ size: Int) -> UnsafeMutablePointer<Void>
@_silgen_name("free") func c_free(_ p: UnsafeMutablePointer<Void>)

class Vector<T> {
  var length : Int
  var capacity : Int
  var base : UnsafeMutablePointer<T>!

  init() {
    length = 0
    capacity = 0
    base = nil
  }

  func push_back(_ elem: T) {
    if length == capacity {
      let newcapacity = capacity * 2 + 2
      let size = Int(Builtin.sizeof(T.self))
      let newbase = UnsafeMutablePointer<T>(c_malloc(newcapacity * size))
      for i in 0..<length {
        (newbase + i).initialize(to: (base+i).move())
      }
      c_free(base)
      base = newbase
      capacity = newcapacity
    }
    (base+length).initialize(to: elem)
    length += 1
  }

  func pop_back() -> T {
    length -= 1
    return (base + length).move()
  }

  subscript (i : Int) -> T {
    get {
      if i >= length {
        Builtin.int_trap()
      }
      return (base + i).pointee
    }
    set {
      if i >= length {
        Builtin.int_trap()
      }
      (base + i).pointee = newValue
    }
  }

  deinit {
    for i in 0..<length {
      (base + i).deinitialize()
    }
    c_free(base)
  }
}

protocol Comparable {
  static func <(lhs: Self, rhs: Self) -> Bool
}

func sort<T : Comparable>(_ array: inout [T]) {
  for i in 0..<array.count {
    for j in i+1..<array.count {
      if array[j] < array[i] {
        let temp = array[i]
        array[i] = array[j]
        array[j] = temp
      }
    }
  }
}

func find<T : Eq>(_ array: [T], value: T) -> Int {
  var idx = 0
  for elt in array {
     if (elt == value) { return idx }
     idx += 1
  }
  return -1
}

func findIf<T>(_ array: [T], fn: (T) -> Bool) -> Int {
  var idx = 0
  for elt in array {
     if (fn(elt)) { return idx }
     idx += 1
  }
  return -1
}

protocol Eq {
  static func ==(lhs: Self, rhs: Self) -> Bool
  static func !=(lhs: Self, rhs: Self) -> Bool
}
