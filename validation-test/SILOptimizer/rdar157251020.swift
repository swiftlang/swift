// RUN: %target-swift-frontend -emit-sil -O -module-name test %s | %FileCheck %s

// rdar://157251020 (Repeat each generates code that goes through the value witness table)

@available(anyAppleOS 26.0, *)
public protocol VectorArithmetic {
    mutating func scale(by rhs: Double)
}

@available(anyAppleOS 26.0, *)
public extension VectorArithmetic {
    func scaled(by rhs: Double) -> Self {
        var result = self
        result.scale(by: rhs)
        return result
    }
}

@available(anyAppleOS 26.0, *)
public struct AnimatableValues<each Value: VectorArithmetic>: VectorArithmetic {
    public var value: (repeat each Value)
    
    public mutating func scale(by rhs: Double) {
        value = (repeat (each value).scaled(by: rhs))
    }
}

@available(anyAppleOS 26.0, *)
extension Double: VectorArithmetic {
    public mutating func scale(by rhs: Double) { self *= rhs }
}

@available(anyAppleOS 26.0, *)
public typealias Point_ParameterPackNested = AnimatableValues<Double, AnimatableValues<Double>>
@available(anyAppleOS 26.0, *)
public typealias Point_ParameterPackNested2 = AnimatableValues<Double, Point_ParameterPackNested>
@available(anyAppleOS 26.0, *)
public typealias Point_ParameterPackNested3 = AnimatableValues<Double, Point_ParameterPackNested2>
@available(anyAppleOS 26.0, *)
public typealias Point_ParameterPackNested4 = AnimatableValues<Double, Point_ParameterPackNested3>
@available(anyAppleOS 26.0, *)
public typealias Point_ParameterPack5 = AnimatableValues<Double, Double, Double, Double, Double>
@available(anyAppleOS 26.0, *)
public typealias Point_ParameterPack20 = AnimatableValues<Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double>

// Nested packs, and packs with single elements, are fully specialized.
// Recursive specialization and inlining of nested packs is order-sensitive, so
// these test cases would break if either scale(by:) or scaled(by:) were
// annotated @inline(always).
//
// CHECK-LABEL:   sil {{.*}} @$s4test26scaledParameterPackNested2_2byAA16AnimatableValuesVySd_AEySd_AEySd_QPGQPGQPGAHn_SdtF : $@convention(thin) (AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double>>>, Double) -> AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double>>> {
// CHECK-NOT:       = alloc_stack
// CHECK-COUNT-3:   builtin "fmul_FPIEEE64"
// CHECK-NOT:       {{^bb}}
// CHECK-LABEL:   } // end sil function '$s4test26scaledParameterPackNested2_2byAA16AnimatableValuesVySd_AEySd_AEySd_QPGQPGQPGAHn_SdtF'
@available(anyAppleOS 26.0, *)
public func scaledParameterPackNested2(_ lhs: consuming Point_ParameterPackNested2, by rhs: Double) -> Point_ParameterPackNested2 {
    lhs.scale(by: rhs)
    return lhs
}

// CHECK-LABEL:   sil {{.*}} @$s4test26scaledParameterPackNested3_2byAA16AnimatableValuesVySd_AEySd_AEySd_AEySd_QPGQPGQPGQPGAIn_SdtF : $@convention(thin) (AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double>>>>, Double) -> AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double>>>>
// CHECK-NOT:       = alloc_stack
// CHECK-COUNT-4:   builtin "fmul_FPIEEE64"
// CHECK-NOT:       {{^bb}}
// CHECK-LABEL:   } // end sil function '$s4test26scaledParameterPackNested3_2byAA16AnimatableValuesVySd_AEySd_AEySd_AEySd_QPGQPGQPGQPGAIn_SdtF'
@available(anyAppleOS 26.0, *)
public func scaledParameterPackNested3(_ lhs: consuming Point_ParameterPackNested3, by rhs: Double) -> Point_ParameterPackNested3 {
    lhs.scale(by: rhs)
    return lhs
}

// CHECK-LABEL:   sil {{.*}} @$s4test26scaledParameterPackNested4_2byAA16AnimatableValuesVySd_AEySd_AEySd_AEySd_AEySd_QPGQPGQPGQPGQPGAJn_SdtF : $@convention(thin) (AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double>>>>>, Double) -> AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double, AnimatableValues<Double>>>>>
// CHECK-NOT:       = alloc_stack
// CHECK-COUNT-5:   builtin "fmul_FPIEEE64"
// CHECK-NOT:       {{^bb}}
// CHECK-LABEL:   } // end sil function '$s4test26scaledParameterPackNested4_2byAA16AnimatableValuesVySd_AEySd_AEySd_AEySd_AEySd_QPGQPGQPGQPGQPGAJn_SdtF'
@available(anyAppleOS 26.0, *)
public func scaledParameterPackNested4(_ lhs: consuming Point_ParameterPackNested4, by rhs: Double) -> Point_ParameterPackNested4 {
    lhs.scale(by: rhs)
    return lhs
}


// Expansions of large homogeneous packs are fully specialized.

// CHECK-LABEL:   sil {{.*}} @$s4test24scaledParameterPackFlat5_2byAA16AnimatableValuesVySd_S4dQPGAFn_SdtF : $@convention(thin) (AnimatableValues<Double, Double, Double, Double, Double>, Double) -> AnimatableValues<Double, Double, Double, Double, Double> {
// CHECK-NOT:       = alloc_stack
// CHECK-COUNT-5:   builtin "fmul_FPIEEE64"
// CHECK-NOT:       {{^bb}}
// CHECK-LABEL:   } // end sil function '$s4test24scaledParameterPackFlat5_2byAA16AnimatableValuesVySd_S4dQPGAFn_SdtF'
@available(anyAppleOS 26.0, *)
public func scaledParameterPackFlat5(_ lhs: consuming Point_ParameterPack5, by rhs: Double) -> Point_ParameterPack5 {
    lhs.scale(by: rhs)
    return lhs
}

// A stack allocation is introduced that, for larger packs (c. 7+ Doubles) is
// not eliminated in SIL. Once lowered to LLVM, these larger packs will be
// passed by address, so the stack allocation is easily eliminated.
//
// CHECK-LABEL:   sil {{.*}} @$s4test25scaledParameterPackFlat20_2byAA16AnimatableValuesVySd_S19dQPGAFn_SdtF : $@convention(thin) (AnimatableValues<Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double>, Double) -> AnimatableValues<Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double> {
// CHECK:           alloc_stack [dynamic_lifetime] $(Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)
// CHECK-COUNT-20:  builtin "fmul_FPIEEE64"
// CHECK-NOT:       {{^bb}}
// CHECK-LABEL:   } // end sil function '$s4test25scaledParameterPackFlat20_2byAA16AnimatableValuesVySd_S19dQPGAFn_SdtF'
@available(anyAppleOS 26.0, *)
public func scaledParameterPackFlat20(_ lhs: consuming Point_ParameterPack20, by rhs: Double) -> Point_ParameterPack20 {
    lhs.scale(by: rhs)
    return lhs
}
