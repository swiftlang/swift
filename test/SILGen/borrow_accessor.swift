// RUN:%target-swift-frontend -emit-silgen %s -verify  -enable-experimental-feature BorrowAndMutateAccessors | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

public class Klass {}

func getKlass() -> Klass {
  return Klass()
}

func use(_ t: Klass) {}

public struct NC : ~Copyable {}

func use(_ t: borrowing NC) {}

public struct S {
  var _k: Klass

  var borrowKlass: Klass {
    borrow {
      return _k
    }
  }
  var getKlass: Klass {
    get {
      return _k
    }
  }
  var readKlass: Klass {
    _read {
      yield _k
    }
  }
}

public struct Wrapper {
  var _k: Klass
  var _s: S

  var s: S {
    borrow {
      return _s
    }
  }

  var k: Klass {
    borrow {
      return _k
    }
  }

  var nested_borrow: Klass {
    borrow {
      return _s.borrowKlass
    }
  }

  var nested_get: Klass {
    borrow {
      return _s.getKlass // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }
  
  var nested_read: Klass {
    borrow {
      return _s.readKlass // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }
  
  var owned_value_direct: Klass {
    borrow {
      return getKlass() // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }

  var owned_value_projected: Klass {
    borrow {
      let w = Wrapper(_k: Klass(), _s: S(_k: Klass()))
      return w.k // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }

  var nested: Klass {
    borrow {
      return k
    }
  }

  subscript(index: Int) -> Klass {
    borrow {
      return _k
    }
  }
  
  var nested_subscript: Klass {
    borrow {
      return self[0]
    }
  }
  
  var if_klass: Klass {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return _k
      } 
      return _k
    }
  }

  var tuple_klass: (Klass, Klass) {
    borrow {
      return (_k, _k) // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }

  var literal: Int {
    borrow {
      return 0
    }
  }
  
  var opt_klass: Klass? {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return nil
      } 
      return _k // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }
}

public struct SimpleWrapper<T> {
  var _prop: T

  var borrow_prop: T {
    borrow {
      return _prop
    }
  }
  
  var get_prop: T {
    get {
      return _prop
    }
  }
  
  var read_prop: T {
    _read {
      yield _prop
    }
  }
}

public struct GenWrapper<T> {
  var _prop: T
  var _w: SimpleWrapper<T>
  var _klass: Klass

  public var prop: T {
    borrow {
      return _prop 
    }
  }
  
  var nested_prop: T {
    borrow {
      return _w.borrow_prop
    }
  }

  var get_prop: T {
    borrow {
      return _w.get_prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 

    }
  }
  
  var read_prop: T {
    borrow {
      return _w.read_prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }
  
  var nested: T {
    borrow {
      return prop
    }
  }

  var klass: Klass {
    borrow {
      return _klass
    }
  }

  subscript(index: Int) -> T {
    borrow {
      return _prop
    }
  }
  
  var nested_subscript: T {
    borrow {
      return self[0]
    }
  }
  
  var if_prop: T {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return _prop
      } 
      return _prop // expected-error{{multiple return statements in borrow accessors are not yet supported}} 
    }
  }

  var tuple_prop: (T, T) {
    borrow {
      return (_prop, _prop) // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }

  var literal: Int {
    borrow {
      return 0
    }
  }
  
  var opt_T: T? {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return nil
      } 
      return _prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 

    }
  }
}

public struct SimpleNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T

  var borrow_prop: T {
    borrow {
      return _prop
    }
  }
  
  var get_prop: T {
    get {
      return _prop
    }
  }
  
  var read_prop: T {
    _read {
      yield _prop
    }
  }
}

public struct GenNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T
  var _w: SimpleNCWrapper<T>

  public var prop: T {
    borrow {
      return _prop 
    }
  }
  
  var nested_prop: T {
    borrow {
      return _w.borrow_prop
    }
  }

  var nested_get: T {
    borrow {
      return _w.get_prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 

    }
  }
  
  var nested_read: T {
    borrow {
      return _w.read_prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 

    }
  }
  
  var nested: T {
    borrow {
      return prop
    }
  }

  subscript(index: Int) -> T {
    borrow {
      return _prop
    }
  }
  
  var nested_subscript: T {
    borrow {
      return self[0]
    }
  }
  
  var if_prop: T {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return _prop
      } 
      return _prop // expected-error{{multiple return statements in borrow accessors are not yet supported}}
    }
  }

  var literal: Int {
    borrow {
      return 0
    }
  }
  
  var opt_T: T? {
    borrow {
      if Int.random(in: 1..<100) == 0 {
        return nil
      } 
      return _prop // expected-error{{invalid return value from borrow accessor}} // expected-note{{borrow accessors can return either literals, stored properties or computed properties that have borrow accessors}} 
    }
  }
}


func test() {
  let w1 = Wrapper(_k: Klass(), _s: S(_k: Klass()))
  use(w1.k)
  var k1 = w1.k
  use(k1)
  k1 = Klass()
  use(k1)
  use(w1.nested_borrow)
  use(w1.nested)

  let w2 = GenWrapper(_prop: Klass(), _w: SimpleWrapper(_prop: Klass()), _klass: Klass())
  use(w2.prop)
  var k2 = w2.prop
  use(k2)
  k2 = Klass()
  use(k2)
  use(w2.nested_prop)
  use(w2.nested)
}

func nctest() {
  let w1 = GenNCWrapper(_prop: NC(), _w: SimpleNCWrapper(_prop: NC()))
  use(w1.prop)
  var k2 = w1.prop // MoveOnlyChecker diagnoses the copy
  use(k2)
  k2 = NC()
  use(k2)
}

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV1kAA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   [[REG2:%.*]] = struct_extract [[REG0]], #Wrapper._k             
// CHECK:   return [[REG2]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV07nested_A0AA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   [[REG2:%.*]] = struct_extract [[REG0]], #Wrapper._s             
// CHECK:   [[REG3:%.*]] = function_ref @$s15borrow_accessor1SV0A5KlassAA0C0Cvb : $@convention(method) (@guaranteed S) -> @guaranteed Klass 
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@guaranteed S) -> @guaranteed Klass 
// CHECK:   return [[REG4]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV6nestedAA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   [[REG2:%.*]] = function_ref @$s15borrow_accessor7WrapperV1kAA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass 
// CHECK:   [[REG3:%.*]] = apply [[REG2]]([[REG0]]) : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass 
// CHECK:   return [[REG3]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperVyAA5KlassCSicib : $@convention(method) (Int, @guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : @guaranteed $Wrapper):
// CHECK:   [[REG4:%.*]] = struct_extract [[REG1]], #Wrapper._k             
// CHECK:   return [[REG4]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV16nested_subscriptAA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   [[REG2:%.*]] = integer_literal $Builtin.IntLiteral, 0     
// CHECK:   [[REG3:%.*]] = metatype $@thin Int.Type                   
// CHECK:   [[REG4:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int 
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG2]], [[REG3]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int 
// CHECK:   [[REG6:%.*]] = function_ref @$s15borrow_accessor7WrapperVyAA5KlassCSicib : $@convention(method) (Int, @guaranteed Wrapper) -> @guaranteed Klass 
// CHECK:   [[REG7:%.*]] = apply [[REG6]]([[REG5]], [[REG0]]) : $@convention(method) (Int, @guaranteed Wrapper) -> @guaranteed Klass 
// CHECK:   return [[REG7]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor7WrapperV8if_klassAA5KlassCvb : $@convention(method) (@guaranteed Wrapper) -> @guaranteed Klass {
// CHECK: bb0([[REG0:%.*]] : @guaranteed $Wrapper):
// CHECK:   cond_br {{.*}}, bb1, bb2                           
// CHECK: bb1:                                              
// CHECK:   [[EX1:%.*]] = struct_extract [[REG0]], #Wrapper._k            
// CHECK:   br bb3([[EX1]])                                     
// CHECK: bb2:                                              
// CHECK:   [[EX2:%.*]] = struct_extract [[REG0]], #Wrapper._k            
// CHECK:   br bb3([[EX2]])                                     
// CHECK: bb3([[PHI:%.*]] : @guaranteed $Klass):                    
// CHECK:   return [[PHI]]                                     
// CHECK: } 

// CHECK: sil [ossa] @$s15borrow_accessor10GenWrapperV4propxvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed_addr T {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._prop  
// CHECK:   return [[REG2]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV11nested_propxvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed_addr T {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #GenWrapper._w     
// CHECK:   [[REG3:%.*]] = function_ref @$s15borrow_accessor13SimpleWrapperV0A5_propxvb : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_addr τ_0_0 
// CHECK:   [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0> (@in_guaranteed SimpleWrapper<τ_0_0>) -> @guaranteed_addr τ_0_0 
// CHECK:   return [[REG4]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV6nestedxvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed_addr T {
// CHECK: bb0([[REG0:%.*]] : $*GenWrapper<T>):
// CHECK:   [[REG2:%.*]] = function_ref @$s15borrow_accessor10GenWrapperV4propxvb : $@convention(method) <τ_0_0> (@in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed_addr τ_0_0 
// CHECK:   [[REG3:%.*]] = apply [[REG2]]<T>([[REG0]]) : $@convention(method) <τ_0_0> (@in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed_addr τ_0_0 
// CHECK:   return [[REG3]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperVyxSicib : $@convention(method) <T> (Int, @in_guaranteed GenWrapper<T>) -> @guaranteed_addr T {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*GenWrapper<T>):
// CHECK:   [[REG4:%.*]] = struct_element_addr [[REG1]], #GenWrapper._prop  
// CHECK:   return [[REG4]]                                       
// CHECK: } 

// CHECK: sil hidden [ossa] @$s15borrow_accessor10GenWrapperV16nested_subscriptxvb : $@convention(method) <T> (@in_guaranteed GenWrapper<T>) -> @guaranteed_addr T {
// CHECK: bb0([[REG0]] : $*GenWrapper<T>):
// CHECK:   [[REG2:%.*]] = integer_literal $Builtin.IntLiteral, 0     
// CHECK:   [[REG3:%.*]] = metatype $@thin Int.Type                   
// CHECK:   [[REG4:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int 
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG2]], [[REG3]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int 
// CHECK:   [[REG6:%.*]] = function_ref @$s15borrow_accessor10GenWrapperVyxSicib : $@convention(method) <τ_0_0> (Int, @in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed_addr τ_0_0 
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<T>([[REG5]], [[REG0]]) : $@convention(method) <τ_0_0> (Int, @in_guaranteed GenWrapper<τ_0_0>) -> @guaranteed_addr τ_0_0 
// CHECK:   return [[REG7]]                                       
// CHECK: } 
