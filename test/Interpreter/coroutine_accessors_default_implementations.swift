// RUN: %empty-directory(%t)

// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:     %t/Library.swift   \
// RUN:     %t/LibImpl.underscored.swift   \
// RUN:     -emit-module       \
// RUN:     -module-name Library \
// RUN:     -parse-as-library  \
// RUN:     -enable-library-evolution \
// RUN:     -emit-module-path %t/Library.swiftmodule \
// RUN:     -validate-tbd-against-ir=none

// RUN: %target-swift-frontend \
// RUN:     %t/Executable.swift \
// RUN:     -c \
// RUN:     -parse-as-library \
// RUN:     -module-name Executable \
// RUN:     -I %t \
// RUN:     -o %t/Executable.o \
// RUN:     -validate-tbd-against-ir=none

// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) \
// RUN:     %t/Library.swift \
// RUN:     %t/LibImpl.nonunderscored.swift   \
// RUN:     -emit-module \
// RUN:     -enable-library-evolution \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -emit-module-path %t/Library.swiftmodule \
// RUN:     -module-name Library \
// RUN:     -Xfrontend -validate-tbd-against-ir=none

// RUN: %target-build-swift \
// RUN:     %t/Executable.o \
// RUN:     -lLibrary \
// RUN:     -L %t \
// RUN:     %target-rpath(%t) \
// RUN:     -o %t/main

// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(Library) | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: executable_test

//--- Library.swift
public protocol P {
  @_borrowed
  var i : Int { get set }
  @_borrowed
  subscript(i : Int) -> Int { get set }
}

public func readFromSomeP<T : P>(_ p: T) {
  print(#function, "p.i", p.i)
  print(#function, "p[5]", p[5])
}

public func increment(int: inout Int) {
  print(#function, "before increment", int)
  int += 1
  print(#function, "after increment", int)
}

public func modifySomeP<T : P>(_ p: inout T) {
  print(#function, "begin")
  increment(int: &p.i)
  increment(int: &p[5])
  print(#function, "end")
}

public func getSomeP() -> some P {
  return I()
}

public func getAnyP() -> any P {
  return I()
}

//--- LibImpl.underscored.swift

struct I : P {
  var i : Int = 0
  subscript(i : Int) -> Int { 
    _read {
      let i = 0
      yield i
    }
    _modify {
      var i = 0
      yield &i
    }
  }
}

//--- LibImpl.nonunderscored.swift

struct I : P {
  var _iImpl : Int
  var i : Int {
    read {
      print(#function, "before yield", _iImpl)
      yield _iImpl
      print(#function, "after yield", _iImpl)
    }
    modify {
      print(#function, "before yield", _iImpl)
      yield &_iImpl
      print(#function, "after yield", _iImpl)
    }
  }
  var _sImpl: [Int]
  subscript(i : Int) -> Int {
    read {
      print(#function, "before yield", _sImpl[i])
      yield _sImpl[i]
      print(#function, "after yield", _sImpl[i])
    }
    modify {
      print(#function, "before yield", _sImpl[i])
      yield &_sImpl[i]
      print(#function, "after yield", _sImpl[i])
    }
  }
  init() {
    self._iImpl = 42
    self._sImpl = [1, 1, 2, 3, 5, 8, 13, 21]
  }
}

//--- Executable.swift
import Library

public struct S : P {
  var _iImpl : Int
  public var i : Int {
    _read {
      print(#function, "before yield", _iImpl)
      yield _iImpl
      print(#function, "after yield", _iImpl)
    }
    _modify {
      print(#function, "before yield", _iImpl)
      yield &_iImpl
      print(#function, "after yield", _iImpl)
    }
  }
  var _sImpl: [Int]
  public subscript(i : Int) -> Int {
    _read {
      print(#function, "before yield", _sImpl[i])
      yield _sImpl[i]
      print(#function, "after yield", _sImpl[i])
    }
    _modify {
      print(#function, "before yield", _sImpl[i])
      yield &_sImpl[i]
      print(#function, "after yield", _sImpl[i])
    }
  }
  init() {
    self._iImpl = 42
    self._sImpl = [1, 1, 2, 3, 5, 8, 13, 21]
  }
}

func readFromSomePLocal<T : P>(_ p: T) {
  print(#function, "p.i", p.i)
  print(#function, "p[5]", p[5])
}

func modifySomePLocal<T : P>(_ p: inout T) {
  print(#function, "begin")
  increment(int: &p.i)
  increment(int: &p[5])
  print(#function, "end")
}

@main struct M {
  static func main() {
    var s = S()
// CHECK:      i before yield 42
// CHECK-NEXT: i after yield 42
// CHECK-NEXT: readFromSomeP(_:) p.i 42
// CHECK-NEXT: subscript(_:) before yield 8
// CHECK-NEXT: subscript(_:) after yield 8
// CHECK-NEXT: readFromSomeP(_:) p[5] 8
    readFromSomeP(s)
// CHECK-NEXT: modifySomeP(_:) begin
// CHECK-NEXT: i before yield 42
// CHECK-NEXT: increment(int:) before increment 42
// CHECK-NEXT: increment(int:) after increment 43
// CHECK-NEXT: i after yield 43
// CHECK-NEXT: subscript(_:) before yield 8
// CHECK-NEXT: increment(int:) before increment 8
// CHECK-NEXT: increment(int:) after increment 9
// CHECK-NEXT: subscript(_:) after yield 9
// CHECK-NEXT: modifySomeP(_:) end
    modifySomeP(&s)
// CHECK-NEXT: modifySomePLocal(_:) begin
// CHECK-NEXT: i before yield 43
// CHECK-NEXT: increment(int:) before increment 43
// CHECK-NEXT: increment(int:) after increment 44
// CHECK-NEXT: i after yield 44
// CHECK-NEXT: subscript(_:) before yield 9
// CHECK-NEXT: increment(int:) before increment 9
// CHECK-NEXT: increment(int:) after increment 10
// CHECK-NEXT: subscript(_:) after yield 10
// CHECK-NEXT: modifySomePLocal(_:) end
    modifySomePLocal(&s)
// CHECK-NEXT: i before yield 44
// CHECK-NEXT: i after yield 44
// CHECK-NEXT: readFromSomePLocal(_:) p.i 44
// CHECK-NEXT: subscript(_:) before yield 10
// CHECK-NEXT: subscript(_:) after yield 10
// CHECK-NEXT: readFromSomePLocal(_:) p[5] 10
    readFromSomePLocal(s)
    var sp = getSomeP()
// CHECK-NEXT: i before yield 42
// CHECK-NEXT: i after yield 42
// CHECK-NEXT: readFromSomePLocal(_:) p.i 42
// CHECK-NEXT: subscript(_:) before yield 8
// CHECK-NEXT: subscript(_:) after yield 8
// CHECK-NEXT: readFromSomePLocal(_:) p[5] 8
    readFromSomePLocal(sp)
// CHECK-NEXT: modifySomePLocal(_:) begin
// CHECK-NEXT: i before yield 42
// CHECK-NEXT: increment(int:) before increment 42
// CHECK-NEXT: increment(int:) after increment 43
// CHECK-NEXT: i after yield 43
// CHECK-NEXT: subscript(_:) before yield 8
// CHECK-NEXT: increment(int:) before increment 8
// CHECK-NEXT: increment(int:) after increment 9
// CHECK-NEXT: subscript(_:) after yield 9
// CHECK-NEXT: modifySomePLocal(_:) end
    modifySomePLocal(&sp)
// CHECK-NEXT: modifySomeP(_:) begin
// CHECK-NEXT: i before yield 43
// CHECK-NEXT: increment(int:) before increment 43
// CHECK-NEXT: increment(int:) after increment 44
// CHECK-NEXT: i after yield 44
// CHECK-NEXT: subscript(_:) before yield 9
// CHECK-NEXT: increment(int:) before increment 9
// CHECK-NEXT: increment(int:) after increment 10
// CHECK-NEXT: subscript(_:) after yield 10
// CHECK-NEXT: modifySomeP(_:) end
    modifySomeP(&sp)
// CHECK-NEXT: i before yield 44
// CHECK-NEXT: i after yield 44
// CHECK-NEXT: readFromSomeP(_:) p.i 44
// CHECK-NEXT: subscript(_:) before yield 10
// CHECK-NEXT: subscript(_:) after yield 10
// CHECK-NEXT: readFromSomeP(_:) p[5] 10
    readFromSomeP(sp)
  }
}

