// RUN: %empty-directory(%t)

// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:     %t/Library.swift   \
// RUN:     %t/LibImpl.underscored.swift   \
// RUN:     -enable-callee-allocated-coro-abi \
// RUN:     -emit-module       \
// RUN:     -module-name Library \
// RUN:     -parse-as-library  \
// RUN:     -enable-library-evolution \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend \
// RUN:     %t/Executable.swift \
// RUN:     -enable-callee-allocated-coro-abi \
// RUN:     -c \
// RUN:     -parse-as-library \
// RUN:     -module-name Executable \
// RUN:     -I %t \
// RUN:     -o %t/Executable.o

// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) \
// RUN:     %t/Library.swift \
// RUN:     %t/LibImpl.nonunderscored.swift   \
// RUN:     -Xfrontend -enable-callee-allocated-coro-abi \
// RUN:     -emit-module \
// RUN:     -enable-library-evolution \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -emit-module-path %t/Library.swiftmodule \
// RUN:     -module-name Library

// RUN: %target-build-swift \
// RUN:     %t/Executable.o \
// RUN:     -lLibrary \
// RUN:     -L %t \
// RUN:     %target-rpath(%t) \
// RUN:     -o %t/main

// RUN: %target-codesign %t/main %t/%target-library-name(Library)
// RUN: %target-run %t/main %t/%target-library-name(Library) | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: executable_test

// This test verifies the backwards compatibility of binaries built against old
// SDKs running on newer OSes (where CoroutineAccessors has been enabled).
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

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

public func readSomeB(_ b: B) {
  print(#function, "b.i", b.i)
  print(#function, "b[5]", b[5])
}

public func modifySomeB(_ b: inout B) {
  print(#function, "begin")
  increment(int: &b.i)
  increment(int: &b[5])
  print(#function, "end")
}

public func modifySomeB2(_ b: inout B2) {
  print(#function, "begin")
  increment(int: &b.i)
  increment(int: &b[5])
  print(#function, "end")
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

open class B {
  public init() {}
  @_borrowed
  open var i: Int {
    _read {
      fatalError()
    }
    _modify {
      fatalError()
    }
  }
  @_borrowed
  open subscript(i : Int) -> Int {
    _read {
      fatalError()
    }
    _modify {
      fatalError()
    }
  }
}

open class B2 {
  public init() {}
  open var i: Int {
    get {
      fatalError()
    }
    set {
      fatalError()
    }
    _modify {
      fatalError()
    }
  }
  open subscript(i : Int) -> Int {
    get {
      fatalError()
    }
    set {
      fatalError()
    }
    _modify {
      fatalError()
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

open class B {
  public init() {}
  open var i: Int {
    read {
      fatalError()
    }
    modify {
      fatalError()
    }
  }
  open subscript(i : Int) -> Int {
    read {
      fatalError()
    }
    modify {
      fatalError()
    }
  }
}

open class B2 {
  public init() {}
  open var i: Int {
    get {
      fatalError()
    }
    set {
      fatalError()
    }
    modify {
      fatalError()
    }
  }
  open subscript(i : Int) -> Int {
    get {
      fatalError()
    }
    set {
      fatalError()
    }
    modify {
      fatalError()
    }
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

class D : B {
  var _iImpl : Int
  @_borrowed
  override var i : Int {
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
  @_borrowed
  override subscript(i : Int) -> Int {
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
  override init() {
    self._iImpl = 42
    self._sImpl = [1, 1, 2, 3, 5, 8, 13, 21]
  }
}

class D2 : B2 {
  var _iImpl : Int
  override var i : Int {
    get {
      print(#function, "before get", _sImpl[i])
      return _sImpl[i]
    }
    set {
      print(#function, "before set", _sImpl[i])
      _sImpl[i] = newValue
      print(#function, "after set", _sImpl[i])
    }
    _modify {
      print(#function, "before yield", _iImpl)
      yield &_iImpl
      print(#function, "after yield", _iImpl)
    }
  }
  var _sImpl: [Int]
  override subscript(i : Int) -> Int {
    get {
      print(#function, "before get", _sImpl[i])
      return _sImpl[i]
    }
    set {
      print(#function, "before set", _sImpl[i])
      _sImpl[i] = newValue
      print(#function, "after set", _sImpl[i])
    }
    _modify {
      print(#function, "before yield", _sImpl[i])
      yield &_sImpl[i]
      print(#function, "after yield", _sImpl[i])
    }
  }
  override init() {
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

func getSomeBLocal() -> B {
  return D()
}

public func getSomeB2Local() -> B2 {
  return D2()
}

func readSomeBLocal(_ b: B) {
  print(#function, "p.i", b.i)
  print(#function, "p[5]", b[5])
}

func modifySomeBLocal(_ b: inout B) {
  print(#function, "begin")
  increment(int: &b.i)
  increment(int: &b[5])
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
    var sb = getSomeBLocal()
// CHECK-NEXT: i before yield 42
// CHECK-NEXT: i after yield 42
// CHECK-NEXT: readSomeB(_:) b.i 42
// CHECK-NEXT: subscript(_:) before yield 8
// CHECK-NEXT: subscript(_:) after yield 8
// CHECK-NEXT: readSomeB(_:) b[5] 8
    readSomeB(sb)
// CHECK-NEXT: modifySomeB(_:) begin
// CHECK-NEXT: i before yield 42
// CHECK-NEXT: increment(int:) before increment 42
// CHECK-NEXT: increment(int:) after increment 43
// CHECK-NEXT: i after yield 43
// CHECK-NEXT: subscript(_:) before yield 8
// CHECK-NEXT: increment(int:) before increment 8
// CHECK-NEXT: increment(int:) after increment 9
// CHECK-NEXT: subscript(_:) after yield 9
// CHECK-NEXT: modifySomeB(_:) end
    modifySomeB(&sb)
    var sb2 = getSomeB2Local()
// CHECK-NEXT: modifySomeB2(_:) begin
// CHECK-NEXT: i before yield 42
// CHECK-NEXT: increment(int:) before increment 42
// CHECK-NEXT: increment(int:) after increment 43
// CHECK-NEXT: i after yield 43
// CHECK-NEXT: subscript(_:) before yield 8
// CHECK-NEXT: increment(int:) before increment 8
// CHECK-NEXT: increment(int:) after increment 9
// CHECK-NEXT: subscript(_:) after yield 9
// CHECK-NEXT: modifySomeB2(_:) end
    modifySomeB2(&sb2)
  }
}

