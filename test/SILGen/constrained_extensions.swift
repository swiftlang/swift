// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s
// RUN: %target-swift-frontend -emit-ir -O %s

extension Array where Element == Int {
  // CHECK-LABEL: sil @_TFe22constrained_extensionsRxzSirSaCfT1xT__GSax_ : $@convention(method) (@thin Array<Int>.Type) -> @owned Array<Int>
  public init(x: ()) {
    self.init()
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensionsRxzSirSag16instancePropertySi : $@convention(method) (@guaranteed Array<Int>) -> Int
  // CHECK-LABEL: sil @_TFe22constrained_extensionsRxzSirSas16instancePropertySi : $@convention(method) (Int, @inout Array<Int>) -> ()
  // CHECK-LABEL: sil [transparent] [fragile] @_TFe22constrained_extensionsRxzSirSam16instancePropertySi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Array<Int>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  // CHECK-LABEL: sil [transparent] [fragile] @_TFFe22constrained_extensionsRxzSirSam16instancePropertySiU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Array<Int>, @thick Array<Int>.Type) -> ()
  public var instanceProperty: Element {
    get {
      return self[0]
    }
    set {
      self[0] = newValue
    }
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensionsRxzSirSa14instanceMethodfT_x : $@convention(method) (@guaranteed Array<Int>) -> Int
  public func instanceMethod() -> Element {
    return instanceProperty
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensionsRxzSirSa14instanceMethodfT1ex_x : $@convention(method) (Int, @guaranteed Array<Int>) -> Int
  public func instanceMethod(e: Element) -> Element {
    return e
  }

  // CHECK-LABEL: sil @_TZFe22constrained_extensionsRxzSirSag14staticPropertySi : $@convention(method) (@thin Array<Int>.Type) -> Int
  public static var staticProperty: Element {
    return Array(x: ()).instanceProperty
  }

  // CHECK-LABEL: sil @_TZFe22constrained_extensionsRxzSirSa12staticMethodfT_x : $@convention(method) (@thin Array<Int>.Type) -> Int
  public static func staticMethod() -> Element {
    return staticProperty
  }

  // CHECK-LABEL: sil @_TIZFe22constrained_extensionsRxzSirSa12staticMethodFT1eGSqx__xA_ : $@convention(thin) () -> Optional<Int>
  // CHECK-LABEL: sil @_TZFe22constrained_extensionsRxzSirSa12staticMethodfT1eGSqx__x : $@convention(method) (Optional<Int>, @thin Array<Int>.Type) -> Int
  public static func staticMethod(e: Element? = nil) -> Element {
    return e!
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensionsRxzSirSag9subscriptFT_Si : $@convention(method) (@guaranteed Array<Int>) -> Int
  public subscript(i: ()) -> Element {
    return self[0]
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensionsRxzSirSa21inoutAccessOfPropertyfT_T_ : $@convention(method) (@inout Array<Int>) -> ()
  public mutating func inoutAccessOfProperty() {
    func increment(x: inout Element) {
      x += 1
    }

    increment(x: &instanceProperty)
  }
}

extension Dictionary where Key == Int {
  // CHECK-LABEL: sil @_TFe22constrained_extensions0_RxzSirVs10DictionaryCfT1xT__GS0_xq__ : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> @owned Dictionary<Int, Value> {
  public init(x: ()) {
    self.init()
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensions0_RxzSirVs10Dictionaryg16instancePropertyq_ : $@convention(method) <Key, Value where Key == Int> (@guaranteed Dictionary<Int, Value>) -> @out Value
  // CHECK-LABEL: sil @_TFe22constrained_extensions0_RxzSirVs10Dictionarys16instancePropertyq_ : $@convention(method) <Key, Value where Key == Int> (@in Value, @inout Dictionary<Int, Value>) -> ()
  // CHECK-LABEL: sil [transparent] [fragile] @_TFe22constrained_extensions0_RxzSirVs10Dictionarym16instancePropertyq_ : $@convention(method) <Key, Value where Key == Int> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Dictionary<Int, Value>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  // CHECK-LABEL: sil [transparent] [fragile] @_TFFe22constrained_extensions0_RxzSirVs10Dictionarym16instancePropertyq_U_T_ : $@convention(thin) <Key, Value where Key == Int> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Dictionary<Int, Value>, @thick Dictionary<Int, Value>.Type) -> ()
  public var instanceProperty: Value {
    get {
      return self[0]!
    }
    set {
      self[0] = newValue
    }
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensions0_RxzSirVs10Dictionary14instanceMethodfT_q_ : $@convention(method) <Key, Value where Key == Int> (@guaranteed Dictionary<Int, Value>) -> @out Value
  public func instanceMethod() -> Value {
    return instanceProperty
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensions0_RxzSirVs10Dictionary14instanceMethodfT1vq__q_ : $@convention(method) <Key, Value where Key == Int> (@in Value, @guaranteed Dictionary<Int, Value>) -> @out Value
  public func instanceMethod(v: Value) -> Value {
    return v
  }

  // CHECK-LABEL: sil @_TZFe22constrained_extensions0_RxzSirVs10Dictionary12staticMethodfT_x : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> Int
  public static func staticMethod() -> Key {
    return staticProperty
  }

  // CHECK-LABEL: sil @_TZFe22constrained_extensions0_RxzSirVs10Dictionaryg14staticPropertySi : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> Int
  public static var staticProperty: Key {
    return 0
  }

  // CHECK-LABEL: sil @_TIZFe22constrained_extensions0_RxzSirVs10Dictionary12staticMethodFT1kGSqx_1vGSqq___q_A_ : $@convention(thin) <Key, Value where Key == Int> () -> Optional<Int>
  // CHECK-LABEL: sil @_TIZFe22constrained_extensions0_RxzSirVs10Dictionary12staticMethodFT1kGSqx_1vGSqq___q_A0_ : $@convention(thin) <Key, Value where Key == Int> () -> @out Optional<Value>
  // CHECK-LABEL: sil @_TZFe22constrained_extensions0_RxzSirVs10Dictionary12staticMethodfT1kGSqx_1vGSqq___q_ : $@convention(method) <Key, Value where Key == Int> (Optional<Int>, @in Optional<Value>, @thin Dictionary<Int, Value>.Type) -> @out Value
  public static func staticMethod(k: Key? = nil, v: Value? = nil) -> Value {
    return v!
  }

  // CHECK-LABEL: sil @_TZFe22constrained_extensions0_RxzSirVs10Dictionary17callsStaticMethodfT_q_ : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> @out Value
  public static func callsStaticMethod() -> Value {
    return staticMethod()
  }

  // CHECK-LABEL: sil @_TZFe22constrained_extensions0_RxzSirVs10Dictionary16callsConstructorfT_q_ : $@convention(method) <Key, Value where Key == Int> (@thin Dictionary<Int, Value>.Type) -> @out Value
  public static func callsConstructor() -> Value {
    return Dictionary(x: ()).instanceMethod()
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensions0_RxzSirVs10Dictionaryg9subscriptFT_q_ : $@convention(method) <Key, Value where Key == Int> (@guaranteed Dictionary<Int, Value>) -> @out Value
  public subscript(i: ()) -> Value {
    return self[0]!
  }

  // CHECK-LABEL: sil @_TFe22constrained_extensions0_RxzSirVs10Dictionary21inoutAccessOfPropertyfT_T_ : $@convention(method) <Key, Value where Key == Int> (@inout Dictionary<Int, Value>) -> ()
  public mutating func inoutAccessOfProperty() {
    func increment(x: inout Value) { }

    increment(x: &instanceProperty)
  }
}
