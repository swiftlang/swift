// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s
// RUN: %target-swift-frontend -emit-ir -O %s

extension Array where Element == Int {
  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlESaySiGyt1x_tcfC : $@convention(method) (@thin Array<Int>.Type) -> @owned Array<Int>
  public init(x: ()) {
    self.init()
  }

  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE16instancePropertySifg : $@convention(method) (@guaranteed Array<Int>) -> Int
  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE16instancePropertySifs : $@convention(method) (Int, @inout Array<Int>) -> ()
  // CHECK-LABEL: sil [transparent] [fragile] @_T0Sa22constrained_extensionsSiRszlE16instancePropertySifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Array<Int>, @thick Array<Int>.Type) -> ()
  // CHECK-LABEL: sil [transparent] [fragile] @_T0Sa22constrained_extensionsSiRszlE16instancePropertySifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Array<Int>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)

  public var instanceProperty: Element {
    get {
      return self[0]
    }
    set {
      self[0] = newValue
    }
  }

  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE14instanceMethodSiyF : $@convention(method) (@guaranteed Array<Int>) -> Int
  public func instanceMethod() -> Element {
    return instanceProperty
  }

  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE14instanceMethodSiSi1e_tF : $@convention(method) (Int, @guaranteed Array<Int>) -> Int
  public func instanceMethod(e: Element) -> Element {
    return e
  }

  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE14staticPropertySifgZ : $@convention(method) (@thin Array<Int>.Type) -> Int
  public static var staticProperty: Element {
    return Array(x: ()).instanceProperty
  }

  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE12staticMethodSiyFZ : $@convention(method) (@thin Array<Int>.Type) -> Int
  public static func staticMethod() -> Element {
    return staticProperty
  }

  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE12staticMethodSiSiSg1e_tFZfA_ : $@convention(thin) () -> Optional<Int>
  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE12staticMethodSiSiSg1e_tFZ : $@convention(method) (Optional<Int>, @thin Array<Int>.Type) -> Int
  public static func staticMethod(e: Element? = nil) -> Element {
    return e!
  }

  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE9subscriptSiycfg : $@convention(method) (@guaranteed Array<Int>) -> Int
  public subscript(i: ()) -> Element {
    return self[0]
  }

  // CHECK-LABEL: sil @_T0Sa22constrained_extensionsSiRszlE21inoutAccessOfPropertyyyF : $@convention(method) (@inout Array<Int>) -> ()
  public mutating func inoutAccessOfProperty() {
    func increment(x: inout Element) {
      x += 1
    }

    increment(x: &instanceProperty)
  }
}

extension Dictionary where Key == Int {
  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lEABySiq_Gyt1x_tcfC : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> @owned Dictionary<Int, Value> {
  public init(x: ()) {
    self.init()
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE16instancePropertyq_fg : $@convention(method) <Key, Value where Key == Int> (@guaranteed Dictionary<Int, Value>) -> @out Value
  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE16instancePropertyq_fs : $@convention(method) <Key, Value where Key == Int> (@in Value, @inout Dictionary<Int, Value>) -> ()
  // CHECK-LABEL: sil [transparent] [fragile] @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE16instancePropertyq_fmytfU_ : $@convention(method) <Key, Value where Key == Int> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Dictionary<Int, Value>, @thick Dictionary<Int, Value>.Type) -> ()
  // CHECK-LABEL: sil [transparent] [fragile] @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE16instancePropertyq_fm : $@convention(method) <Key, Value where Key == Int> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Dictionary<Int, Value>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public var instanceProperty: Value {
    get {
      return self[0]!
    }
    set {
      self[0] = newValue
    }
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE14instanceMethodq_yF : $@convention(method) <Key, Value where Key == Int> (@guaranteed Dictionary<Int, Value>) -> @out Value
  public func instanceMethod() -> Value {
    return instanceProperty
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE14instanceMethodq_q_1v_tF : $@convention(method) <Key, Value where Key == Int> (@in Value, @guaranteed Dictionary<Int, Value>) -> @out Value
  public func instanceMethod(v: Value) -> Value {
    return v
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE12staticMethodSiyFZ : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> Int
  public static func staticMethod() -> Key {
    return staticProperty
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE14staticPropertySifgZ : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> Int
  public static var staticProperty: Key {
    return 0
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE12staticMethodq_SiSg1k_q_Sg1vtFZfA_ : $@convention(thin) <Key, Value where Key == Int> () -> Optional<Int>
  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE12staticMethodq_SiSg1k_q_Sg1vtFZfA0_ : $@convention(thin) <Key, Value where Key == Int> () -> @out Optional<Value>
  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE12staticMethodq_SiSg1k_q_Sg1vtFZ : $@convention(method) <Key, Value where Key == Int> (Optional<Int>, @in Optional<Value>, @thin Dictionary<Int, Value>.Type) -> @out Value
  public static func staticMethod(k: Key? = nil, v: Value? = nil) -> Value {
    return v!
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE17callsStaticMethodq_yFZ : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> @out Value
  public static func callsStaticMethod() -> Value {
    return staticMethod()
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE16callsConstructorq_yFZ : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> @out Value
  public static func callsConstructor() -> Value {
    return Dictionary(x: ()).instanceMethod()
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE9subscriptq_ycfg : $@convention(method) <Key, Value where Key == Int> (@guaranteed Dictionary<Int, Value>) -> @out Value
  public subscript(i: ()) -> Value {
    return self[0]!
  }

  // CHECK-LABEL: sil @_T0s10DictionaryV22constrained_extensionsSiRszr0_lE21inoutAccessOfPropertyyyF : $@convention(method) <Key, Value where Key == Int> (@inout Dictionary<Int, Value>) -> ()
  public mutating func inoutAccessOfProperty() {
    func increment(x: inout Value) { }

    increment(x: &instanceProperty)
  }
}
