// RUN: %target-swift-emit-silgen %s | %FileCheck %s

struct Subscript1 {
  subscript(_: Int = 0) -> Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s27subscript_default_arguments10Subscript1VyS2icipfA_ : $@convention(thin) () -> Int {

struct Subscript2 {
  subscript(_ x: Int = 0) -> Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s27subscript_default_arguments10Subscript2VyS2icipfA_ : $@convention(thin) () -> Int {

struct Subscript3 {
  subscript(x x: Int = 0) -> Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s27subscript_default_arguments10Subscript3V1xS2i_tcipfA_ : $@convention(thin) () -> Int {

struct Subscript4 {
  subscript(_ x: Int, y y: Int = 0) -> Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s27subscript_default_arguments10Subscript4V_1yS2i_SitcipfA0_ : $@convention(thin) () -> Int {

struct Subscript5 {
  subscript(x x: Int, y y: Int = 0) -> Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s27subscript_default_arguments10Subscript5V1x1yS2i_SitcipfA0_ : $@convention(thin) () -> Int {

func defaultArg<T>() -> T? { return nil }

struct Subscript6<T> {
  subscript(x x: Int = 0, y y: T? = defaultArg()) -> Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s27subscript_default_arguments10Subscript6V1x1yS2i_xSgtcipfA_ : $@convention(thin) <T> () -> Int {
// CHECK-LABEL: sil hidden [ossa] @$s27subscript_default_arguments10Subscript6V1x1yS2i_xSgtcipfA0_ : $@convention(thin) <T> () -> @out Optional<T> {

struct Subscript7<T> {
  subscript<U>(x x: T? = defaultArg(), y y: U) -> Int {
    get {}
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s27subscript_default_arguments10Subscript7V1x1ySixSg_qd__tcluipfA_ : $@convention(thin) <T><U> () -> @out Optional<T> {

struct Subscript8 {
  subscript(file: String = #file, line: Int = #line, function: String = #function) -> Int {
    get {}
    set {}
  }
}

public protocol P {
  associatedtype T
  static func f() -> T
}

// Doesn't work yet
#if false
// FIXME-LABEL: sil hidden [ossa] @$s27subscript_default_arguments1PPAAEySi1TQzcipfA_ : $@convention(thin) <Self where Self : P> () -> @out Self.T {

extension P {
  subscript(t: T = f()) -> Int {
    get {}
    set {}
  }
}

struct Subscript9<T> : P {
  static func f() -> T {}
}
#endif

// CHECK-LABEL: sil [ossa] @$s27subscript_default_arguments10subscript1yyF : $@convention(thin) () -> () {
public func subscript1() {
  var s = Subscript1()

  _ = s[]
  s[] = 0
  s[] += 1
}

public func subscript2() {
  var s = Subscript2()

  _ = s[]
  s[] = 0
  s[] += 1
}

public func subscript3() {
  var s = Subscript3()

  _ = s[]
  s[] = 0
  s[] += 1
}

public func subscript4() {
  var s = Subscript4()

  _ = s[0]
  s[0] = 0
  s[0] += 1
}

public func subscript5() {
  var s = Subscript5()

  _ = s[x: 0]
  s[x: 0] = 0
  s[x: 0] += 1
}

public func subscript6() {
  var s = Subscript6<String>()

  _ = s[]
  s[] = 0
  s[] += 1
}

public func subscript7() {
  var s = Subscript7<String>()

  _ = s[y: 123]
  s[y: 123] = 0
  s[y: 123] += 1
}

public func subscript8() {
  var s = Subscript8()

  _ = s[]
  s[] = 0
  s[] += 1
}

#if false
public func subscript9() {
  var s = Subscript9<Int>()

  _ = s[]
  s[] = 0
  s[] += 1
}
#endif
