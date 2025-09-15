// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -verify -Xllvm --sil-print-after=differentiation -Xllvm --debug-only=differentiation %s 2>&1 | %FileCheck %s

// Needed for '--debug-only'
// REQUIRES: asserts

import _Differentiation

//===----------------------------------------------------------------------===//
// Pullback generation - `struct_extract`
// - Nested AddElement adjoint kind is created due to the extraction of same
// field multiple times
//===----------------------------------------------------------------------===//

struct SmallTestModel : Differentiable {
    public var stored1: Float = 3.0
    public var stored2: Float = 3.0
    public var stored3: Float = 3.0
}

@differentiable(reverse)
func multipleExtractionOfSameField(_ model: SmallTestModel) -> Float{
    return model.stored1 + model.stored1 + model.stored1
}

// CHECK-LABEL: [AD] Accumulating adjoint directly.
// CHECK: LHS: AddElement[Zero[$SmallTestModel.TangentVector], Field(stored1), Concrete[$Float]((%5, **%6**) = destructure_tuple %3 : $(Float, Float)
// CHECK: )]
// CHECK: RHS: AddElement[Zero[$SmallTestModel.TangentVector], Field(stored1), Concrete[$Float]((%9, **%10**) = destructure_tuple %7 : $(Float, Float)
// CHECK: )]

// CHECK-LABEL: [AD] Accumulating adjoint directly.
// CHECK: LHS: AddElement[AddElement[Zero[$SmallTestModel.TangentVector], Field(stored1), Concrete[$Float]((%5, **%6**) = destructure_tuple %3 : $(Float, Float)
// CHECK: )], Field(stored1), Concrete[$Float]((%9, **%10**) = destructure_tuple %7 : $(Float, Float)
// CHECK: )]
// CHECK: RHS: AddElement[Zero[$SmallTestModel.TangentVector], Field(stored1), Concrete[$Float]((**%9**, %10) = destructure_tuple %7 : $(Float, Float)
// CHECK: )]

// CHECK-LABEL: sil private [ossa] @$s46pullback_generation_nested_addelement_adjoints29multipleExtractionOfSameFieldySfAA14SmallTestModelVFTJpSpSr : $@convention(thin) (Float, @owned @callee_guaranteed (Float) -> (Float, Float), @owned @callee_guaranteed (Float) -> (Float, Float)) -> SmallTestModel.TangentVector {
// CHECK: bb0(%0 : $Float, %1 : @owned $@callee_guaranteed (Float) -> (Float, Float), %2 : @owned $@callee_guaranteed (Float) -> (Float, Float)):
// CHECK:   %3 = apply %2(%0) : $@callee_guaranteed (Float) -> (Float, Float) 
// CHECK:   destroy_value %2 : $@callee_guaranteed (Float) -> (Float, Float) 
// CHECK:   (%5, %6) = destructure_tuple %3 : $(Float, Float) 
// CHECK:   %7 = apply %1(%5) : $@callee_guaranteed (Float) -> (Float, Float) 
// CHECK:   destroy_value %1 : $@callee_guaranteed (Float) -> (Float, Float) 
// CHECK:   (%9, %10) = destructure_tuple %7 : $(Float, Float) 
// CHECK:   %11 = alloc_stack $SmallTestModel.TangentVector 
// CHECK:   %12 = witness_method $SmallTestModel.TangentVector, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0 
// CHECK:   %13 = metatype $@thick SmallTestModel.TangentVector.Type 
// CHECK:   %14 = apply %12<SmallTestModel.TangentVector>(%11, %13) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   %15 = struct_element_addr %11 : $*SmallTestModel.TangentVector, #SmallTestModel.TangentVector.stored1 
// CHECK:   %16 = alloc_stack $Float                        
// CHECK:   store %9 to [trivial] %16 : $*Float             
// CHECK:   %18 = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %19 = metatype $@thick Float.Type               
// CHECK:   %20 = apply %18<Float>(%15, %16, %19) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %16 : $*Float                      
// CHECK:   dealloc_stack %16 : $*Float                     
// CHECK:   %23 = struct_element_addr %11 : $*SmallTestModel.TangentVector, #SmallTestModel.TangentVector.stored1 
// CHECK:   %24 = alloc_stack $Float                        
// CHECK:   store %10 to [trivial] %24 : $*Float            
// CHECK:   %26 = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %27 = metatype $@thick Float.Type               
// CHECK:   %28 = apply %26<Float>(%23, %24, %27) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %24 : $*Float                      
// CHECK:   dealloc_stack %24 : $*Float                     
// CHECK:   %31 = struct_element_addr %11 : $*SmallTestModel.TangentVector, #SmallTestModel.TangentVector.stored1 
// CHECK:   %32 = alloc_stack $Float                        
// CHECK:   store %6 to [trivial] %32 : $*Float             
// CHECK:   %34 = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %35 = metatype $@thick Float.Type               
// CHECK:   %36 = apply %34<Float>(%31, %32, %35) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %32 : $*Float                      
// CHECK:   dealloc_stack %32 : $*Float                     
// CHECK:   %39 = load [trivial] %11 : $*SmallTestModel.TangentVector 
// CHECK:   dealloc_stack %11 : $*SmallTestModel.TangentVector 
// CHECK:   debug_value %39 : $SmallTestModel.TangentVector, let, name "model", argno 1 
// CHECK:   return %39 : $SmallTestModel.TangentVector      
// CHECK: }

//===----------------------------------------------------------------------===//
// Pullback generation - `struct_extract`
// - Nested AddElement adjoint kind is created due to the extraction of multiple 
// fields from the same struct
//===----------------------------------------------------------------------===//

@differentiable(reverse)
func multipleExtractionsFromSameStruct(_ model: SmallTestModel) -> Float{
    return model.stored1 + model.stored2 + model.stored3
}

// CHECK-LABEL: [AD] Accumulating adjoint directly.
// CHECK: LHS: AddElement[Zero[$SmallTestModel.TangentVector], Field(stored3), Concrete[$Float]((%5, **%6**) = destructure_tuple %3 : $(Float, Float)
// CHECK: )]
// CHECK: RHS: AddElement[Zero[$SmallTestModel.TangentVector], Field(stored2), Concrete[$Float]((%9, **%10**) = destructure_tuple %7 : $(Float, Float)
// CHECK: )]

// CHECK-LABEL: [AD] Accumulating adjoint directly.
// CHECK: LHS: AddElement[AddElement[Zero[$SmallTestModel.TangentVector], Field(stored3), Concrete[$Float]((%5, **%6**) = destructure_tuple %3 : $(Float, Float)
// CHECK: )], Field(stored2), Concrete[$Float]((%9, **%10**) = destructure_tuple %7 : $(Float, Float)
// CHECK: )]
// CHECK: RHS: AddElement[Zero[$SmallTestModel.TangentVector], Field(stored1), Concrete[$Float]((**%9**, %10) = destructure_tuple %7 : $(Float, Float)
// CHECK: )]

// CHECK-LABEL: sil private [ossa] @$s46pullback_generation_nested_addelement_adjoints33multipleExtractionsFromSameStructySfAA14SmallTestModelVFTJpSpSr : $@convention(thin) (Float, @owned @callee_guaranteed (Float) -> (Float, Float), @owned @callee_guaranteed (Float) -> (Float, Float)) -> SmallTestModel.TangentVector {
// CHECK: bb0(%0 : $Float, %1 : @owned $@callee_guaranteed (Float) -> (Float, Float), %2 : @owned $@callee_guaranteed (Float) -> (Float, Float)):
// CHECK:   %3 = apply %2(%0) : $@callee_guaranteed (Float) -> (Float, Float) 
// CHECK:   destroy_value %2 : $@callee_guaranteed (Float) -> (Float, Float) 
// CHECK:   (%5, %6) = destructure_tuple %3 : $(Float, Float) 
// CHECK:   %7 = apply %1(%5) : $@callee_guaranteed (Float) -> (Float, Float) 
// CHECK:   destroy_value %1 : $@callee_guaranteed (Float) -> (Float, Float) 
// CHECK:   (%9, %10) = destructure_tuple %7 : $(Float, Float) 
// CHECK:   %11 = alloc_stack $SmallTestModel.TangentVector 
// CHECK:   %12 = witness_method $SmallTestModel.TangentVector, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0 
// CHECK:   %13 = metatype $@thick SmallTestModel.TangentVector.Type 
// CHECK:   %14 = apply %12<SmallTestModel.TangentVector>(%11, %13) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   %15 = struct_element_addr %11 : $*SmallTestModel.TangentVector, #SmallTestModel.TangentVector.stored1 
// CHECK:   %16 = alloc_stack $Float                        
// CHECK:   store %9 to [trivial] %16 : $*Float             
// CHECK:   %18 = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %19 = metatype $@thick Float.Type               
// CHECK:   %20 = apply %18<Float>(%15, %16, %19) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %16 : $*Float                      
// CHECK:   dealloc_stack %16 : $*Float                     
// CHECK:   %23 = struct_element_addr %11 : $*SmallTestModel.TangentVector, #SmallTestModel.TangentVector.stored2 
// CHECK:   %24 = alloc_stack $Float                        
// CHECK:   store %10 to [trivial] %24 : $*Float            
// CHECK:   %26 = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %27 = metatype $@thick Float.Type               
// CHECK:   %28 = apply %26<Float>(%23, %24, %27) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %24 : $*Float                      
// CHECK:   dealloc_stack %24 : $*Float                     
// CHECK:   %31 = struct_element_addr %11 : $*SmallTestModel.TangentVector, #SmallTestModel.TangentVector.stored3 
// CHECK:   %32 = alloc_stack $Float                        
// CHECK:   store %6 to [trivial] %32 : $*Float             
// CHECK:   %34 = witness_method $Float, #AdditiveArithmetic."+=" : <Self where Self : AdditiveArithmetic> (Self.Type) -> (inout Self, Self) -> () : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> () 
// CHECK:   %35 = metatype $@thick Float.Type               
// CHECK:   %36 = apply %34<Float>(%31, %32, %35) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@inout τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> ()
// CHECK:   destroy_addr %32 : $*Float                      
// CHECK:   dealloc_stack %32 : $*Float                     
// CHECK:   %39 = load [trivial] %11 : $*SmallTestModel.TangentVector 
// CHECK:   dealloc_stack %11 : $*SmallTestModel.TangentVector 
// CHECK:   debug_value %39 : $SmallTestModel.TangentVector, let, name "model", argno 1 
// CHECK:   return %39 : $SmallTestModel.TangentVector      
// CHECK: }
