// Pullback generation tests written in Swift

// RUN: %target-swift-frontend -emit-sil -verify -Xllvm --sil-print-after=differentiation %s 2>&1 | %FileCheck %s

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
