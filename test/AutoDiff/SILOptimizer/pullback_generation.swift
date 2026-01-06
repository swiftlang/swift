// Pullback generation tests written in Swift

// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -verify -Xllvm --sil-print-after=differentiation %s 2>&1 | %FileCheck %s

import _Differentiation

//===----------------------------------------------------------------------===//
// Pullback generation - `struct_extract`
// - Operand is piecewise materializable
//===----------------------------------------------------------------------===//
struct PiecewiseMaterializable: Differentiable, Equatable, AdditiveArithmetic {
    public typealias TangentVector = Self
    
    var a: Float
    var b: Double
}

@differentiable(reverse)
func f1(v: PiecewiseMaterializable) -> Float {
    v.a
}

// CHECK-LABEL: sil private [ossa] @$s19pullback_generation2f11vSfAA23PiecewiseMaterializableV_tFTJpSpSr : $@convention(thin) (Float) -> PiecewiseMaterializable {
// CHECK: bb0(%0 : $Float):
// CHECK:   %1 = alloc_stack $PiecewiseMaterializable       
// CHECK:   %2 = witness_method $PiecewiseMaterializable, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0 
// CHECK:   %3 = metatype $@thick PiecewiseMaterializable.Type 
// CHECK:   %4 = apply %2<PiecewiseMaterializable>(%1, %3) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   %5 = struct_element_addr %1 : $*PiecewiseMaterializable, #PiecewiseMaterializable.a 
// CHECK:   %6 = alloc_stack $Float                         
// CHECK:   store %0 to [trivial] %6 : $*Float              
// CHECK:   %8 = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %9 = metatype $@thick Float.Type                
// CHECK:   %10 = apply %8<Float>(%5, %6, %9) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %6 : $*Float                       
// CHECK:   dealloc_stack %6 : $*Float                      
// CHECK:   %13 = load [trivial] %1 : $*PiecewiseMaterializable 
// CHECK:   dealloc_stack %1 : $*PiecewiseMaterializable    
// CHECK:   debug_value %13 : $PiecewiseMaterializable, let, name "v", argno 1 
// CHECK:   return %13 : $PiecewiseMaterializable           
// CHECK: } // end sil function '$s19pullback_generation2f11vSfAA23PiecewiseMaterializableV_tFTJpSpSr'

//===----------------------------------------------------------------------===//
// Pullback generation - `struct_extract`
// - Operand is non-piecewise materializable
//===----------------------------------------------------------------------===//
struct NonPiecewiseMaterializable {
    var a: Float
    var b: String
}

extension NonPiecewiseMaterializable: Differentiable, Equatable, AdditiveArithmetic {
    public typealias TangentVector = Self
    mutating func move(by offset: TangentVector) {fatalError()}
    public static var zero: Self {fatalError()}
    public static func + (lhs: Self, rhs: Self) -> Self {fatalError()}
    public static func - (lhs: Self, rhs: Self) -> Self {fatalError()}
}

@differentiable(reverse)
func f2(v: NonPiecewiseMaterializable) -> Float {
    v.a   
}

// CHECK-LABEL: sil private [ossa] @$s19pullback_generation2f21vSfAA26NonPiecewiseMaterializableV_tFTJpSpSr : $@convention(thin) (Float) -> @owned NonPiecewiseMaterializable {
// CHECK: bb0(%0 : $Float):
// CHECK:   %1 = alloc_stack $NonPiecewiseMaterializable    
// CHECK:   %2 = witness_method $NonPiecewiseMaterializable, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0 
// CHECK:   %3 = metatype $@thick NonPiecewiseMaterializable.Type 
// CHECK:   %4 = apply %2<NonPiecewiseMaterializable>(%1, %3) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   %5 = struct_element_addr %1 : $*NonPiecewiseMaterializable, #NonPiecewiseMaterializable.a 
// CHECK:   %6 = alloc_stack $Float                         
// CHECK:   store %0 to [trivial] %6 : $*Float              
// CHECK:   %8 = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %9 = metatype $@thick Float.Type                
// CHECK:   %10 = apply %8<Float>(%5, %6, %9) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %6 : $*Float                       
// CHECK:   dealloc_stack %6 : $*Float                      
// CHECK:   %13 = load [take] %1 : $*NonPiecewiseMaterializable 
// CHECK:   dealloc_stack %1 : $*NonPiecewiseMaterializable 
// CHECK:   debug_value %13 : $NonPiecewiseMaterializable, let, name "v", argno 1 
// CHECK:   %16 = copy_value %13 : $NonPiecewiseMaterializable 
// CHECK:   destroy_value %13 : $NonPiecewiseMaterializable 
// CHECK:   return %16 : $NonPiecewiseMaterializable        
// CHECK: } // end sil function '$s19pullback_generation2f21vSfAA26NonPiecewiseMaterializableV_tFTJpSpSr'

//===----------------------------------------------------------------------===//
// Pullback generation - `struct_extract`
// - Operand is non-piecewise materializable with an aggregate, piecewise 
// materializable, differentiable field
//===----------------------------------------------------------------------===//
struct NonPiecewiseMaterializableWithAggDifferentiableField {
    var a: PiecewiseMaterializable
    var b: String
}

extension NonPiecewiseMaterializableWithAggDifferentiableField: Differentiable, Equatable, AdditiveArithmetic {
    public typealias TangentVector = Self
    mutating func move(by offset: TangentVector) {fatalError()}
    public static var zero: Self {fatalError()}
    public static func + (lhs: Self, rhs: Self) -> Self {fatalError()}
    public static func - (lhs: Self, rhs: Self) -> Self {fatalError()}
}

@differentiable(reverse)
func f3(v: NonPiecewiseMaterializableWithAggDifferentiableField) -> PiecewiseMaterializable {   
    v.a
}

// CHECK-LABEL: sil private [ossa] @$s19pullback_generation2f31vAA23PiecewiseMaterializableVAA03NondE26WithAggDifferentiableFieldV_tFTJpSpSr : $@convention(thin) (PiecewiseMaterializable) -> @owned NonPiecewiseMaterializableWithAggDifferentiableField {
// CHECK: bb0(%0 : $PiecewiseMaterializable):
// CHECK:   %1 = alloc_stack $NonPiecewiseMaterializableWithAggDifferentiableField 
// CHECK:   %2 = witness_method $NonPiecewiseMaterializableWithAggDifferentiableField, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0 
// CHECK:   %3 = metatype $@thick NonPiecewiseMaterializableWithAggDifferentiableField.Type 
// CHECK:   %4 = apply %2<NonPiecewiseMaterializableWithAggDifferentiableField>(%1, %3) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   %5 = struct_element_addr %1 : $*NonPiecewiseMaterializableWithAggDifferentiableField, #NonPiecewiseMaterializableWithAggDifferentiableField.a 
// CHECK:   %6 = alloc_stack $PiecewiseMaterializable       
// CHECK:   store %0 to [trivial] %6 : $*PiecewiseMaterializable 
// CHECK:   %8 = witness_method $PiecewiseMaterializable, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %9 = metatype $@thick PiecewiseMaterializable.Type 
// CHECK:   %10 = apply %8<PiecewiseMaterializable>(%5, %6, %9) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %6 : $*PiecewiseMaterializable     
// CHECK:   dealloc_stack %6 : $*PiecewiseMaterializable    
// CHECK:   %13 = load [take] %1 : $*NonPiecewiseMaterializableWithAggDifferentiableField 
// CHECK:   dealloc_stack %1 : $*NonPiecewiseMaterializableWithAggDifferentiableField 
// CHECK:   debug_value %13 : $NonPiecewiseMaterializableWithAggDifferentiableField, let, name "v", argno 1 
// CHECK:   %16 = copy_value %13 : $NonPiecewiseMaterializableWithAggDifferentiableField 
// CHECK:   destroy_value %13 : $NonPiecewiseMaterializableWithAggDifferentiableField 
// CHECK:   return %16 : $NonPiecewiseMaterializableWithAggDifferentiableField 
// CHECK: } // end sil function '$s19pullback_generation2f31vAA23PiecewiseMaterializableVAA03NondE26WithAggDifferentiableFieldV_tFTJpSpSr'

//===----------------------------------------------------------------------===//
// Pullback generation - `struct_extract`
// - Adjoint of extracted element can be `AddElement`
// - Just need to make sure that we are able to generate a pullback for B.x's 
// getter
//===----------------------------------------------------------------------===//
struct A: Differentiable {
    public var x: Float
}

struct B: Differentiable {
    var y: A
    
    public init(a: A) {
        self.y = a
    }
    
    @differentiable(reverse)
    public var x: Float {
        get { return self.y.x }
    }
}

// CHECK-LABEL: sil private [ossa] @$s19pullback_generation1BV1xSfvgTJpSpSr : $@convention(thin) (Float) -> B.TangentVector {

//===----------------------------------------------------------------------===//
// Pullback generation - Inner values of concrete adjoints must be copied 
// during indirect materialization
//===----------------------------------------------------------------------===//

struct NonTrivial {
    var x: Float
    var y: String
}

extension NonTrivial: Differentiable, Equatable, AdditiveArithmetic {
    public typealias TangentVector = Self
    mutating func move(by offset: TangentVector) {fatalError()}
    public static var zero: Self {fatalError()}
    public static func + (lhs: Self, rhs: Self) -> Self {fatalError()}
    public static func - (lhs: Self, rhs: Self) -> Self {fatalError()}
}

@differentiable(reverse)
func f4(a: NonTrivial) -> Float {
    var sum: Float = 0
    for _ in 0..<1 {
        sum += a.x
    }
    return sum
}

// CHECK-LABEL: sil private [ossa] @$s19pullback_generation2f41aSfAA10NonTrivialV_tFTJpSpSr : $@convention(thin) (Float, @guaranteed Builtin.NativeObject) -> @owned NonTrivial {
// CHECK: bb5(%[[#ARG0:]] : @owned $NonTrivial, %[[#]] : $Float, %[[#]] : @owned $(predecessor: _AD__$s19pullback_generation2f41aSfAA10NonTrivialV_tF_bb2__Pred__src_0_wrt_0, @callee_guaranteed (@inout Float) -> Float)):
// CHECK: %[[#T0:]] = alloc_stack $NonTrivial

// Non-trivial value must be copied or there will be a
// double consume when all owned parameters are destroyed 
// at the end of the basic block.
// CHECK: %[[#T1:]] = copy_value %[[#ARG0]] : $NonTrivial

// CHECK: store %[[#T1]] to [init] %[[#T0]] : $*NonTrivial
// CHECK: %[[#T2:]] = struct_element_addr %[[#T0]] : $*NonTrivial, #NonTrivial.x
// CHECK: %[[#T3:]] = alloc_stack $Float
// CHECK: store %[[#T4:]] to [trivial] %[[#T3]] : $*Float
// CHECK: %[[#T5:]] = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK: %[[#T6:]] = metatype $@thick Float.Type
// CHECK: %[[#]] = apply %[[#T5]]<Float>(%[[#T2]], %[[#T3]], %[[#T6]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK: destroy_value %[[#ARG0]] : $NonTrivial

@differentiable(reverse)
func move_value(x: Float) -> Float {
  var result = x
  repeat {
    let temp = result
    result = temp
  } while 0 == 1
  return result
}

// CHECK-LABEL: sil private [ossa] @$s19pullback_generation10move_value1xS2f_tFTJpSpSr : $@convention(thin) (Float, @guaranteed Builtin.NativeObject) -> Float {
// CHECK:       bb3(%[[#]] : $Float, %[[#]] : $Float, %[[#]] : $Float, %[[#]] : $(predecessor: _AD__$s19pullback_generation10move_value1xS2f_tF_bb1__Pred__src_0_wrt_0)):
// CHECK:         %[[#]] = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:         %[[#T1:]] = alloc_stack $Float
// CHECK:         %[[#T2:]] = witness_method $Float, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:         %[[#T3:]] = metatype $@thick Float.Type
// CHECK:         %[[#]] = apply %[[#T2]]<Float>(%[[#T1]], %[[#T3]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:         %[[#T4:]] = load [trivial] %[[#T1]]
// CHECK:         dealloc_stack %[[#T1]]
// CHECK:       bb4(%[[#]] : $Builtin.RawPointer):
// CHECK:         br bb5(%[[#]] : $Float, %[[#]] : $Float, %[[#T4]] : $Float, %[[#]] : $(predecessor: _AD__$s19pullback_generation10move_value1xS2f_tF_bb2__Pred__src_0_wrt_0))
