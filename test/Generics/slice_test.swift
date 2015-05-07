// RUN: %target-parse-verify-swift -parse-stdlib

import Swift

infix operator < {
  associativity none
  precedence 170
}

infix operator == {
  associativity none
  precedence 160
}

infix operator != {
  associativity none
  precedence 160
}

func testslice(s: Array<Int>) {
  for i in 0..<s.count() { print(s[i]+1) }
  for i in s { print(i+1) }
  var s2 = s[0..<2]
  var s3 = s[0...1]
}

@asmname("malloc") func c_malloc(size: Int) -> UnsafeMutablePointer<Void>
@asmname("free") func c_free(p: UnsafeMutablePointer<Void>)

class Vector<T> {
  var length : Int
  var capacity : Int
  var base : UnsafeMutablePointer<T>

  init() {
    length = 0
    capacity = 0
    base = nil
  }

  func push_back(elem: T) {
    if length == capacity {
      var newcapacity = capacity * 2 + 2
      var size = Int(Builtin.sizeof(T.self))
      var newbase = UnsafeMutablePointer<T>(c_malloc(newcapacity * size))
      for i in 0..<length {
        (newbase + i).initialize((base+i).move())
      }
      c_free(base)
      base = newbase
      capacity = newcapacity
    }
    (base+length).initialize(elem)
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
      return (base + i).memory
    }
    set {
      if i >= length {
        Builtin.int_trap()
      }
      (base + i).memory = newValue
    }
  }

  deinit {
    for i in 0..<length {
      (base + i).destroy()
    }
    c_free(base)
  }
}

protocol Comparable {
  func <(lhs: Self, rhs: Self) -> Bool
}

func sort<T : Comparable>(inout array: [T]) {
  for i in 0..<array.count() {
    for j in i+1..<array.count() {
      if array[j] < array[i] {
        var temp = array[i]
        array[i] = array[j]
        array[j] = temp
      }
    }
  }
}

func find<T : Eq>(array: [T], value: T) -> Int {
  var idx = 0
  for elt in array {
     if (elt == value) { return idx }
     ++idx
  }
  return -1
}

func findIf<T>(array: [T], fn: (T) -> Bool) -> Int {
  var idx = 0
  for elt in array {
     if (fn(elt)) { return idx }
     ++idx
  }
  return -1
}

protocol Eq {
  func ==(lhs: Self, rhs: Self) -> Bool
  func !=(lhs: Self, rhs: Self) -> Bool
}
