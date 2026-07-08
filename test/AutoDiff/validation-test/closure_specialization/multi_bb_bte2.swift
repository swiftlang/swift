// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(closure_spec_module)) %S/Inputs/closure_spec_module.swift \
// RUN:   -emit-module -emit-module-path %t/closure_spec_module.swiftmodule -module-name closure_spec_module
// RUN: %target-build-swift -I%t -L%t %s -lclosure_spec_module %target-rpath(%t) -o %t/none.out -Onone
// RUN: %target-build-swift -I%t -L%t %s -lclosure_spec_module %target-rpath(%t) -o %t/opt.out  -O
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out

/// Particular optimizations are checked in SILOptimizer tests, here we only check that optimizations occur
// RUN: %target-swift-frontend -emit-sil -I%t %s -o %t/out.sil -O

// RUN: not grep "^// pullback of myfoo01" %t/out.sil
// RUN: not grep "^// pullback of myfoo02" %t/out.sil
// RUN: not grep "^// pullback of myfoo03" %t/out.sil
// RUN: not grep "^// pullback of myfoo04" %t/out.sil
// RUN: not grep "^// pullback of myfoo05" %t/out.sil
// RUN: not grep "^// pullback of myfoo06" %t/out.sil
// RUN: not grep "^// pullback of myfoo07" %t/out.sil
// RUN: not grep "^// pullback of myfoo08" %t/out.sil
// RUN: not grep "^// pullback of myfoo09" %t/out.sil
// RUN: not grep "^// pullback of myfoo10" %t/out.sil
// RUN: not grep "^// pullback of myfoo11" %t/out.sil
// RUN: not grep "^// pullback of myfoo12" %t/out.sil
// RUN: not grep "^// pullback of myfoo13" %t/out.sil
// RUN: not grep "^// pullback of myfoo14" %t/out.sil
// RUN: not grep "^// pullback of myfoo15" %t/out.sil
// RUN: not grep "^// pullback of myfoo16" %t/out.sil
// RUN: not grep "^// pullback of myfoo17" %t/out.sil
// RUN: not grep "^// pullback of myfoo18" %t/out.sil

import DifferentiationUnittest
import StdlibUnittest
import closure_spec_module

#if canImport(Glibc)
  import Glibc
#elseif canImport(Android)
  import Android
#else
  import Foundation
#endif

var AutoDiffClosureSpecializationTests = TestSuite("AutoDiffClosureSpecialization")

extension Dictionary: Differentiable where Value: Differentiable {
    public typealias TangentVector = [Key: Value.TangentVector]
    public mutating func move(by direction: TangentVector) {
        for (componentKey, componentDirection) in direction {
            func fatalMissingComponent() -> Value {
                fatalError("missing component \(componentKey) in moved Dictionary")
            }
            self[componentKey, default: fatalMissingComponent()].move(by: componentDirection)
        }
    }

    public var zeroTangentVectorInitializer: () -> TangentVector {
        let listOfKeys = self.keys // capturing only what's needed, not the entire self, in order to not waste memory
        func initializer() -> Self.TangentVector {
            return listOfKeys.reduce(into: [Key: Value.TangentVector]()) { $0[$1] = Value.TangentVector.zero }
        }
        return initializer
    }
}

extension Dictionary: AdditiveArithmetic where Value: AdditiveArithmetic {
    public static func + (_ lhs: Self, _ rhs: Self) -> Self {
        return lhs.merging(rhs, uniquingKeysWith: +)
    }

    public static func - (_ lhs: Self, _ rhs: Self) -> Self {
        return lhs.merging(rhs.mapValues { .zero - $0 }, uniquingKeysWith: +)
    }

    public static var zero: Self { [:] }
}

extension Dictionary where Value: Differentiable {
    // get
    @usableFromInline
    @derivative(of: subscript(_:))
    func vjpSubscriptGet(key: Key) -> (value: Value?, pullback: (Optional<Value>.TangentVector) -> Dictionary<Key, Value>.TangentVector) {
        // When adding two dictionaries, nil values are equivalent to zeroes, so there is no need to manually zero-out
        // every key's value. Instead, it is faster to create a dictionary with the single non-zero entry.
        return (self[key], { v in
            if let value = v.value {
                return [key: value]
            }
            else {
                return .zero
            }
        })
    }
 }

public extension Dictionary where Value: Differentiable {
    @differentiable(reverse)
    mutating func set(_ key: Key, to newValue: Value) {
        self[key] = newValue
    }

    @derivative(of: set)
    mutating func vjpUpdated(_ key: Key, to newValue: Value) -> (value: Void, pullback: (inout TangentVector) -> (Value.TangentVector)) {
        self.set(key, to: newValue)

        let forwardCount = self.count
        let forwardKeys = self.keys // may be heavy to capture all of these, not sure how to do without them though

        return ((), { v in
            // manual zero tangent initialization
            if v.count < forwardCount {
                v = Self.TangentVector()
                forwardKeys.forEach { v[$0] = .zero }
            }

            if let dElement = v[key] {
                v[key] = .zero
                return dElement
            }
            else { // should this fail?
                v[key] = .zero
                return .zero
            }
        })
    }
}


AutoDiffClosureSpecializationTests.testWithLeakChecking("Test01") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK01 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK01-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK01-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK01-ENUM3 %s

  // CHECK01-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU_7myfoo01L_yS2fF_bb1__Pred__src_0_wrt_0_spec {
  // CHECK01-ENUM1-NEXT:    case bb0(())
  // CHECK01-ENUM1-NEXT:  }

  // CHECK01-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU_7myfoo01L_yS2fF_bb2__Pred__src_0_wrt_0_spec {
  // CHECK01-ENUM2-NEXT:    case bb0(())
  // CHECK01-ENUM2-NEXT:  }

  // CHECK01-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU_7myfoo01L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3_bb1_3 {
  // CHECK01-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU_7myfoo01L_yS2fF_bb2__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK01-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU_7myfoo01L_yS2fF_bb1__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, ()))
  // CHECK01-ENUM3-NEXT:  }

  // CHECK01-LABEL: {{^}}// reverse-mode derivative of myfoo01
  // CHECK01-NEXT:  // Isolation: nonisolated
  // CHECK01-NEXT:  sil private @$s3outyycfU_7myfoo01L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK01:         // function_ref specialized pullback of myfoo01
  // CHECK01:         %[[#T42:]] = function_ref @$s3outyycfU_7myfoo01L_yS2fFTJpSpSr019$_AD__$s3outyycfU_7B30L_yS2fF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU_7myfoo01L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3_bb1_3) -> Float // user: %[[#T43:]]
  // CHECK01:         %[[#T43]] = partial_apply [callee_guaranteed] %[[#T42]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU_7myfoo01L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3_bb1_3) -> Float // user: %[[#T44:]]
  // CHECK01:         %[[#T44]] = tuple (%[[#]], %[[#T43]])                          // user: %[[#T45:]]
  // CHECK01:         return %[[#T44]]                                      // id: %[[#T45]]
  // CHECK01:       } // end sil function '$s3outyycfU_7myfoo01L_yS2fFTJrSpSr'

  // CHECK01-LABEL: {{^}}// specialized pullback of myfoo01
  // CHECK01-NEXT:  // Isolation: nonisolated
  // CHECK01-NEXT:  sil private @$s3outyycfU_7myfoo01L_yS2fFTJpSpSr019$_AD__$s3outyycfU_7B30L_yS2fF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU_7myfoo01L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3_bb1_3) -> Float {

  func myfoo01(_ x: Float) -> Float {
    if x > 0 {
      return mybar1(x) + mybar2(x)
    } else {
      return mybar1(x) * mybar2(x)
    }
  }

  func myfoo01_derivative(_ x: Float) -> Float {
    if x > 0 {
      return 2 * x + 1
    } else {
      return 3 * x * x
    }
  }

  for x in -100...100 {
    expectEqual(myfoo01_derivative(Float(x)), gradient(at: Float(x), of: myfoo01))
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test02") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK02 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK02-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK02-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK02-ENUM3 %s

  // CHECK02-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU0_7myfoo02L_yS2fF_bb1__Pred__src_0_wrt_0_spec_bb0_2_3 {
  // CHECK02-ENUM1-NEXT:    case bb0((@callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, (Float, Float), ()))
  // CHECK02-ENUM1-NEXT:  }

  // CHECK02-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU0_7myfoo02L_yS2fF_bb2__Pred__src_0_wrt_0_spec_bb0_2_3 {
  // CHECK02-ENUM2-NEXT:    case bb0((@callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, (Float, Float), ()))
  // CHECK02-ENUM2-NEXT:  }

  // CHECK02-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU0_7myfoo02L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3_bb1_3_5 {
  // CHECK02-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU0_7myfoo02L_yS2fF_bb2__Pred__src_0_wrt_0_spec_bb0_2_3, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK02-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU0_7myfoo02L_yS2fF_bb1__Pred__src_0_wrt_0_spec_bb0_2_3, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, (Float, Float), @callee_guaranteed (Float) -> Float, ()))
  // CHECK02-ENUM3-NEXT:  }

  // CHECK02-LABEL: {{^}}// reverse-mode derivative of myfoo02
  // CHECK02-NEXT:  // Isolation: nonisolated
  // CHECK02-NEXT:  sil private @$s3outyycfU0_7myfoo02L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK02:         // function_ref specialized pullback of myfoo02
  // CHECK02:         %[[#T72:]] = function_ref @$s3outyycfU0_7myfoo02L_yS2fFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2fACS2f0cd1_e3E7_g11Add3lhs3rhsi1_j1_klj1_km1_kN2U_Tf1nnEEc_n020$_AD__$s3outyycfU0_7B30L_yS2fF_bb3__Pred__src_0_wrt_0Tf1bnnnnn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU0_7myfoo02L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3_bb1_3_5, Float, Float, Float, Float) -> Float // user: %[[#T73:]]
  // CHECK02:         %[[#T73]] = partial_apply [callee_guaranteed] %[[#T72]](%[[#]], %0, %[[#]], %[[#]], %[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU0_7myfoo02L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3_bb1_3_5, Float, Float, Float, Float) -> Float // user: %[[#T74:]]
  // CHECK02:         %[[#T74]] = tuple (%[[#]], %[[#T73]])
  // CHECK02:         return %[[#T74]]
  // CHECK02:       } // end sil function '$s3outyycfU0_7myfoo02L_yS2fFTJrSpSr'

  // CHECK02-LABEL: {{^}}// specialized pullback of myfoo02
  // CHECK02-NEXT:  // Isolation: nonisolated
  // CHECK02-NEXT:  sil private @$s3outyycfU0_7myfoo02L_yS2fFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2fACS2f0cd1_e3E7_g11Add3lhs3rhsi1_j1_klj1_km1_kN2U_Tf1nnEEc_n020$_AD__$s3outyycfU0_7B30L_yS2fF_bb3__Pred__src_0_wrt_0Tf1bnnnnn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU0_7myfoo02L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3_bb1_3_5, Float, Float, Float, Float) -> Float {

  func myfoo02(_ x: Float) -> Float {
    let y = mybar1(x) + 37 * mybar2(x)
    var z: Float = 0
    if x > 0 {
      z = mybar1(y) * mybar2(x) + mybar2(y)
    } else {
      z = mybar1(x) * mybar1(y)
    }
    return z * x + y * y
  }

  func myfoo02_derivative(_ x: Float) -> Float {
    if x > 0 {
      return 7030 * x * x * x * x + 5776 * x * x * x + 225 * x * x + 2 * x
    } else {
      return 5624 * x * x * x + 225 * x * x + 2 * x
    }
  }

  for x in -14...6 {
    expectEqual(myfoo02_derivative(Float(x)), gradient(at: Float(x), of: myfoo02))
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test03") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK03 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK03-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK03-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK03-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK03-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK03-ENUM5 %s

  // CHECK03-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb1__Pred__src_0_wrt_0_spec {
  // CHECK03-ENUM1-NEXT:    case bb0(())
  // CHECK03-ENUM1-NEXT:  }

  // CHECK03-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb2__Pred__src_0_wrt_0_spec {
  // CHECK03-ENUM2-NEXT:    case bb0(())
  // CHECK03-ENUM2-NEXT:  }

  // CHECK03-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3 {
  // CHECK03-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb2__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, ()))
  // CHECK03-ENUM3-NEXT:  }

  // CHECK03-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb4__Pred__src_0_wrt_0_spec_bb2_3 {
  // CHECK03-ENUM4-NEXT:    case bb2((predecessor: _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb2__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, ()))
  // CHECK03-ENUM4-NEXT:  }

  // CHECK03-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb5__Pred__src_0_wrt_0_spec_bb4_2_bb3_2_bb1_3 {
  // CHECK03-ENUM5-NEXT:    case bb4((predecessor: _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb4__Pred__src_0_wrt_0_spec_bb2_3, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK03-ENUM5-NEXT:    case bb3((predecessor: _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3, @callee_guaranteed (Float) -> Float, ()))
  // CHECK03-ENUM5-NEXT:    case bb1((predecessor: _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb1__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK03-ENUM5-NEXT:  }

  // CHECK03-LABEL: {{^}}// reverse-mode derivative of myfoo03
  // CHECK03-NEXT:  // Isolation: nonisolated
  // CHECK03-NEXT:  sil private @$s3outyycfU1_7myfoo03L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK03:         // function_ref specialized pullback of myfoo03
  // CHECK03:         %[[#T44:]] = function_ref @$s3outyycfU1_7myfoo03L_yS2fFTJpSpSr020$_AD__$s3outyycfU1_7B30L_yS2fF_bb5__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb5__Pred__src_0_wrt_0_spec_bb4_2_bb3_2_bb1_3) -> Float // user: %[[#T45:]]
  // CHECK03:         %[[#T45]] = partial_apply [callee_guaranteed] %[[#T44]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb5__Pred__src_0_wrt_0_spec_bb4_2_bb3_2_bb1_3) -> Float // user: %[[#T46:]]
  // CHECK03:         %[[#T46]] = tuple (%[[#]], %[[#T45]])                          // user: %[[#T47:]]
  // CHECK03:         return %[[#T46]]                                      // id: %[[#T47]]
  // CHECK03:       } // end sil function '$s3outyycfU1_7myfoo03L_yS2fFTJrSpSr'

  // CHECK03-LABEL: {{^}}// specialized pullback of myfoo03
  // CHECK03-NEXT:  // Isolation: nonisolated
  // CHECK03-NEXT:  sil private @$s3outyycfU1_7myfoo03L_yS2fFTJpSpSr020$_AD__$s3outyycfU1_7B30L_yS2fF_bb5__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU1_7myfoo03L_yS2fF_bb5__Pred__src_0_wrt_0_spec_bb4_2_bb3_2_bb1_3) -> Float {

  func myfoo03(_ x: Float) -> Float {
    if x > 0 {
      return mybar2(x) * mybar2(x)
    } else {
      var y = mybar1(x) + mybar2(x)
      if x > -10 {
        return x + mybar2(y)
      } else {
        return mybar1(y) * x
      }
    }
  }

  func myfoo03_derivative(_ x: Float) -> Float {
    if x > 0 {
      return 4 * x * x * x
    } else {
      if x > -10 {
        return 4 * x * x * x + 6 * x * x + 2 * x + 1
      } else {
        return 3 * x * x + 2 * x
      }
    }
  }

  for x in -100...100 {
    expectEqual(myfoo03_derivative(Float(x)), gradient(at: Float(x), of: myfoo03))
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test04") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK04 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK04-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK04-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK04-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK04-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK04-ENUM5 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK04-ENUM6 %s

  // CHECK04-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb1__Pred__src_0_wrt_0_spec {
  // CHECK04-ENUM1-NEXT:    case bb0(())
  // CHECK04-ENUM1-NEXT:  }

  // CHECK04-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb2__Pred__src_0_wrt_0_spec {
  // CHECK04-ENUM2-NEXT:    case bb0(())
  // CHECK04-ENUM2-NEXT:  }

  // CHECK04-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3 {
  // CHECK04-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb2__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, ()))
  // CHECK04-ENUM3-NEXT:  }

  // CHECK04-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb4__Pred__src_0_wrt_0_spec_bb2_3 {
  // CHECK04-ENUM4-NEXT:    case bb2((predecessor: _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb2__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, ()))
  // CHECK04-ENUM4-NEXT:  }

  // CHECK04-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb5__Pred__src_0_wrt_0_spec_bb4_2_3_bb3_2_3 {
  // CHECK04-ENUM5-NEXT:    case bb4((predecessor: _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb4__Pred__src_0_wrt_0_spec_bb2_3, @callee_guaranteed (Float) -> Float, (Float, Float), ()))
  // CHECK04-ENUM5-NEXT:    case bb3((predecessor: _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_3, @callee_guaranteed (Float) -> Float, (), ()))
  // CHECK04-ENUM5-NEXT:  }

  // CHECK04-ENUM6-LABEL: {{^}}enum _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb6__Pred__src_0_wrt_0_spec_bb5_3_bb1_3 {
  // CHECK04-ENUM6-NEXT:    case bb5((predecessor: _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb5__Pred__src_0_wrt_0_spec_bb4_2_3_bb3_2_3, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK04-ENUM6-NEXT:    case bb1((predecessor: _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb1__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK04-ENUM6-NEXT:  }

  // CHECK04-LABEL: {{^}}// reverse-mode derivative of myfoo04
  // CHECK04-NEXT:  // Isolation: nonisolated
  // CHECK04-NEXT:  sil private @$s3outyycfU2_7myfoo04L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK04:         // function_ref specialized pullback of myfoo04
  // CHECK04:         %[[#T46:]] = function_ref @$s3outyycfU2_7myfoo04L_yS2fFTJpSpSr020$_AD__$s3outyycfU2_7B30L_yS2fF_bb6__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb6__Pred__src_0_wrt_0_spec_bb5_3_bb1_3) -> Float // user: %[[#T47:]]
  // CHECK04:         %[[#T47]] = partial_apply [callee_guaranteed] %[[#T46]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb6__Pred__src_0_wrt_0_spec_bb5_3_bb1_3) -> Float // user: %[[#T48:]]
  // CHECK04:         %[[#T48]] = tuple (%[[#]], %[[#T47]])                          // user: %[[#T49:]]
  // CHECK04:         return %[[#T48]]                                      // id: %[[#T49]]
  // CHECK04:       } // end sil function '$s3outyycfU2_7myfoo04L_yS2fFTJrSpSr'

  // CHECK04-LABEL: {{^}}// specialized pullback of myfoo04
  // CHECK04-NEXT:  // Isolation: nonisolated
  // CHECK04-NEXT:  sil private @$s3outyycfU2_7myfoo04L_yS2fFTJpSpSr020$_AD__$s3outyycfU2_7B30L_yS2fF_bb6__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU2_7myfoo04L_yS2fF_bb6__Pred__src_0_wrt_0_spec_bb5_3_bb1_3) -> Float {

  func myfoo04(_ x: Float) -> Float {
    if x > 0 {
      return mybar2(x) * mybar2(x)
    } else {
      let y = mybar1(x) + mybar2(x)
      var z: Float = 37
      if x > -7 {
        z += y + mybar2(x)
      } else {
        z -= mybar1(x) * y
      }
      return mybar1(z) * mybar2(y)
    }
  }

  func myfoo04_derivative(_ x: Float) -> Float {
    if x > 0 {
      return 4 * x * x * x
    } else if x > -7 {
      return 12 * x * x * x * x * x + 25 * x * x * x * x + 164 * x * x * x + 225 * x * x + 74 * x
    } else {
      return -7 * x * x * x * x * x * x - 18 * x * x * x * x * x - 15 * x * x * x * x + 144 * x * x
        * x + 222 * x * x + 74 * x
    }
  }

  for x in -20...100 {
    expectEqual(myfoo04_derivative(Float(x)), gradient(at: Float(x), of: myfoo04))
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test05") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK05 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK05-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK05-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK05-ENUM3 %s

  // CHECK05-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU3_7myfoo05L_yS2fF_bb1__Pred__src_0_wrt_0_spec {
  // CHECK05-ENUM1-NEXT:    case bb0(())
  // CHECK05-ENUM1-NEXT:  }

  // CHECK05-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU3_7myfoo05L_yS2fF_bb2__Pred__src_0_wrt_0_spec {
  // CHECK05-ENUM2-NEXT:    case bb0(())
  // CHECK05-ENUM2-NEXT:  }

  // CHECK05-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU3_7myfoo05L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_3_bb1_1_2_3 {
  // CHECK05-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU3_7myfoo05L_yS2fF_bb2__Pred__src_0_wrt_0_spec, (_: Float), (_: Float), ()))
  // CHECK05-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU3_7myfoo05L_yS2fF_bb1__Pred__src_0_wrt_0_spec, (_: Float), (_: Float), (Float, Float)))
  // CHECK05-ENUM3-NEXT:  }

  // CHECK05-LABEL: {{^}}// reverse-mode derivative of myfoo05
  // CHECK05-NEXT:  // Isolation: nonisolated
  // CHECK05-NEXT:  sil private @$s3outyycfU3_7myfoo05L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK05:         // function_ref specialized pullback of myfoo05
  // CHECK05:         %[[#T36:]] = function_ref @$s3outyycfU3_7myfoo05L_yS2fFTJpSpSr020$_AD__$s3outyycfU3_7B30L_yS2fF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU3_7myfoo05L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_3_bb1_1_2_3) -> Float // user: %[[#T37:]]
  // CHECK05:         %[[#T37]] = partial_apply [callee_guaranteed] %[[#T36]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU3_7myfoo05L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_3_bb1_1_2_3) -> Float // user: %[[#T38:]]
  // CHECK05:         %[[#T38]] = tuple (%[[#]], %[[#T37]])                          // user: %[[#T39:]]
  // CHECK05:         return %[[#T38]]                                      // id: %[[#T39]]
  // CHECK05:       } // end sil function '$s3outyycfU3_7myfoo05L_yS2fFTJrSpSr'

  // CHECK05-LABEL: {{^}}// specialized pullback of myfoo05
  // CHECK05-NEXT:  // Isolation: nonisolated
  // CHECK05-NEXT:  sil private @$s3outyycfU3_7myfoo05L_yS2fFTJpSpSr020$_AD__$s3outyycfU3_7B30L_yS2fF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU3_7myfoo05L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_3_bb1_1_2_3) -> Float {

  func myfoo05(_ x: Float) -> Float {
    if x > 0 {
      return sin(x) * cos(x)
    } else {
      return sin(x) + cos(x)
    }
  }

  func myfoo05_derivative(_ x: Float) -> Float {
    if x > 0 {
      return cos(x) * cos(x) - sin(x) * sin(x)
    } else {
      return cos(x) - sin(x)
    }
  }

  for x in -100...100 {
    expectEqual(myfoo05_derivative(Float(x)), gradient(at: Float(x), of: myfoo05))
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test06") {
  func double(x: Float) -> Float {
    return x + x
  }

  func square(x: Float) -> Float {
    return x * x
  }

  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM5 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM6 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM7 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM8 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM9 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK06-ENUM10 %s

  // CHECK06-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb1__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM1-NEXT:    case bb0(())
  // CHECK06-ENUM1-NEXT:  }

  // CHECK06-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb2__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM2-NEXT:    case bb1((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb1__Pred__src_0_wrt_0_spec))
  // CHECK06-ENUM2-NEXT:  }

  // CHECK06-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb3__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb2__Pred__src_0_wrt_0_spec))
  // CHECK06-ENUM3-NEXT:  }

  // CHECK06-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb4__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM4-NEXT:    case bb3((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb3__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float))
  // CHECK06-ENUM4-NEXT:  }

  // CHECK06-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb5__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM5-NEXT:    case bb3((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb3__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float))
  // CHECK06-ENUM5-NEXT:  }

  // CHECK06-ENUM6-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb6__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM6-NEXT:    case bb2((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb2__Pred__src_0_wrt_0_spec))
  // CHECK06-ENUM6-NEXT:  }

  // CHECK06-ENUM7-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb7__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM7-NEXT:    case bb1((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb1__Pred__src_0_wrt_0_spec))
  // CHECK06-ENUM7-NEXT:  }

  // CHECK06-ENUM8-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb8__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM8-NEXT:    case bb0(())
  // CHECK06-ENUM8-NEXT:  }

  // CHECK06-ENUM9-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb9__Pred__src_0_wrt_0_spec {
  // CHECK06-ENUM9-NEXT:    case bb6((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb6__Pred__src_0_wrt_0_spec))
  // CHECK06-ENUM9-NEXT:    case bb7((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb7__Pred__src_0_wrt_0_spec))
  // CHECK06-ENUM9-NEXT:  }

  // CHECK06-ENUM10-LABEL: {{^}}enum _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb10__Pred__src_0_wrt_0_spec_bb9_1_bb8_2_bb5_2_3_bb4_2_3 {
  // CHECK06-ENUM10-NEXT:    case bb9((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb9__Pred__src_0_wrt_0_spec, (Float, Float)))
  // CHECK06-ENUM10-NEXT:    case bb8((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb8__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK06-ENUM10-NEXT:    case bb5((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb5__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, (Float, Float), ()))
  // CHECK06-ENUM10-NEXT:    case bb4((predecessor: _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb4__Pred__src_0_wrt_0_spec, @callee_guaranteed (Float) -> Float, (Float, Float), ()))
  // CHECK06-ENUM10-NEXT:  }

  // CHECK06-LABEL: {{^}}// reverse-mode derivative of myfoo06
  // CHECK06-NEXT:  // Isolation: nonisolated
  // CHECK06-NEXT:  sil private @$s3outyycfU4_7myfoo06L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK06:         // function_ref specialized pullback of myfoo06
  // CHECK06:         %[[#T81:]] = function_ref @$s3outyycfU4_7myfoo06L_yS2fFTJpSpSr020$_AD__$s3outyycfU4_7B31L_yS2fF_bb10__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb10__Pred__src_0_wrt_0_spec_bb9_1_bb8_2_bb5_2_3_bb4_2_3) -> Float // user: %[[#T82:]]
  // CHECK06:         %[[#T82]] = partial_apply [callee_guaranteed] %[[#T81]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb10__Pred__src_0_wrt_0_spec_bb9_1_bb8_2_bb5_2_3_bb4_2_3) -> Float // user: %[[#T83:]]
  // CHECK06:         %[[#T83]] = tuple (%[[#]], %[[#T82]])                          // user: %[[#T84:]]
  // CHECK06:         return %[[#T83]]                                      // id: %[[#T84]]
  // CHECK06:       } // end sil function '$s3outyycfU4_7myfoo06L_yS2fFTJrSpSr'

  // CHECK06-LABEL: {{^}}// specialized pullback of myfoo06
  // CHECK06-NEXT:  // Isolation: nonisolated
  // CHECK06-NEXT:  sil private @$s3outyycfU4_7myfoo06L_yS2fFTJpSpSr020$_AD__$s3outyycfU4_7B31L_yS2fF_bb10__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU4_7myfoo06L_yS2fF_bb10__Pred__src_0_wrt_0_spec_bb9_1_bb8_2_bb5_2_3_bb4_2_3) -> Float {

  func myfoo06(_ x: Float) -> Float {
    if x > 0 {
      if (x + 1) < 5 {
        if x * 2 > 4 {
          let y = square(x: x)
          if y >= x {
            let d = double(x: x)
            return x - (d * y)
          } else {
            let e = square(x: y)
            return x + (e * y)
          }
        }
      }
    } else {
      let y = double(x: x)
      return x * y
    }

    return x * 3
  }

  func myfoo06_derivative(_ x: Float) -> Float {
    if x > 2 && x < 4 {
      return 1 - 6 * x * x
    } else if x > 0 {
      return 3
    } else {
      return 4 * x
    }
  }

  for x in -100...100 {
    expectEqual(myfoo06_derivative(Float(x)), gradient(at: Float(x), of: myfoo06))
  }

  var x : Float = -10
  while x < 10 {
    let a = myfoo06_derivative(Float(x))
    let b = gradient(at: Float(x), of: myfoo06)
    expectEqual((a * 10000).rounded(), (b * 10000).rounded())
    x += 1 / 1024
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test07") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK07 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK07-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK07-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK07-ENUM3 %s

  // CHECK07-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU5_7myfoo07L_yS2f_SbtF_bb1__Pred__src_0_wrt_0_spec_bb0_0 {
  // CHECK07-ENUM1-NEXT:    case bb0((_: (Float, Float)))
  // CHECK07-ENUM1-NEXT:  }

  // CHECK07-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU5_7myfoo07L_yS2f_SbtF_bb2__Pred__src_0_wrt_0_spec_bb0_0 {
  // CHECK07-ENUM2-NEXT:    case bb0((_: (Float, Float)))
  // CHECK07-ENUM2-NEXT:  }

  // CHECK07-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU5_7myfoo07L_yS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_2 {
  // CHECK07-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU5_7myfoo07L_yS2f_SbtF_bb2__Pred__src_0_wrt_0_spec_bb0_0, (Float, Float)))
  // CHECK07-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU5_7myfoo07L_yS2f_SbtF_bb1__Pred__src_0_wrt_0_spec_bb0_0, (), ()))
  // CHECK07-ENUM3-NEXT:  }

  // CHECK07-LABEL: {{^}}// reverse-mode derivative of myfoo07
  // CHECK07-NEXT:  // Isolation: nonisolated
  // CHECK07-NEXT:  sil private @$s3outyycfU5_7myfoo07L_yS2f_SbtFTJrSUpSr : $@convention(thin) (Float, Bool) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK07:         // function_ref specialized pullback of myfoo07
  // CHECK07:         %[[#T34:]] = function_ref @$s3outyycfU5_7myfoo07L_yS2f_SbtFTJpSUpSr020$_AD__$s3outyycfU5_7B34L_yS2f_SbtF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU5_7myfoo07L_yS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_2) -> Float // user: %[[#T35:]]
  // CHECK07:         %[[#T35]] = partial_apply [callee_guaranteed] %[[#T34]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU5_7myfoo07L_yS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_2) -> Float // user: %[[#T36:]]
  // CHECK07:         %[[#T36]] = tuple (%[[#]], %[[#T35]])                          // user: %[[#T37:]]
  // CHECK07:         return %[[#T36]]                                      // id: %[[#T37]]
  // CHECK07:       } // end sil function '$s3outyycfU5_7myfoo07L_yS2f_SbtFTJrSUpSr'

  // CHECK07-LABEL: {{^}}// specialized pullback of myfoo07
  // CHECK07-NEXT:  // Isolation: nonisolated
  // CHECK07-NEXT:  sil private @$s3outyycfU5_7myfoo07L_yS2f_SbtFTJpSUpSr020$_AD__$s3outyycfU5_7B34L_yS2f_SbtF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU5_7myfoo07L_yS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_2) -> Float {

  func myfoo07(_ x: Float, _ bool: Bool) -> Float {
    var result = x * x
    if bool {
      result = result + result
      result = result - x
    } else {
      result = result / x
    }
    return result
  }

  func myfoo07_derivative(_ x: Float, _ bool: Bool) -> Float {
    if bool {
      return 4 * x - 1
    }
    return 1
  }

  for x in -100...100 {
    if x == 0 {
      continue
    }
    expectEqual(gradient(at: Float(x), of: { myfoo07(Float($0), true)  }), myfoo07_derivative(Float(x), true))
    expectEqual((100000 * gradient(at: Float(x), of: { myfoo07(Float($0), false) })).rounded(), (100000 * myfoo07_derivative(Float(x), false)).rounded())
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test08") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK08 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK08-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK08-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK08-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK08-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK08-ENUM5 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK08-ENUM6 %s

  // CHECK08-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb1__Pred__src_0_wrt_0_1_spec_bb0_0_1 {
  // CHECK08-ENUM1-NEXT:    case bb0(((), ()))
  // CHECK08-ENUM1-NEXT:  }

  // CHECK08-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb2__Pred__src_0_wrt_0_1_spec_bb0_0_1 {
  // CHECK08-ENUM2-NEXT:    case bb0(((), ()))
  // CHECK08-ENUM2-NEXT:  }

  // CHECK08-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb3__Pred__src_0_wrt_0_1_spec {
  // CHECK08-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb2__Pred__src_0_wrt_0_1_spec_bb0_0_1))
  // CHECK08-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb1__Pred__src_0_wrt_0_1_spec_bb0_0_1))
  // CHECK08-ENUM3-NEXT:  }

  // CHECK08-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb4__Pred__src_0_wrt_0_1_spec_bb3_1 {
  // CHECK08-ENUM4-NEXT:    case bb3((predecessor: _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb3__Pred__src_0_wrt_0_1_spec, (Float, Float)))
  // CHECK08-ENUM4-NEXT:  }

  // CHECK08-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb5__Pred__src_0_wrt_0_1_spec_bb3_1 {
  // CHECK08-ENUM5-NEXT:    case bb3((predecessor: _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb3__Pred__src_0_wrt_0_1_spec, (Float, Float)))
  // CHECK08-ENUM5-NEXT:  }

  // CHECK08-ENUM6-LABEL: {{^}}enum _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb6__Pred__src_0_wrt_0_1_spec {
  // CHECK08-ENUM6-NEXT:    case bb5((predecessor: _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb5__Pred__src_0_wrt_0_1_spec_bb3_1))
  // CHECK08-ENUM6-NEXT:    case bb4((predecessor: _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb4__Pred__src_0_wrt_0_1_spec_bb3_1))
  // CHECK08-ENUM6-NEXT:  }

  // CHECK08-LABEL: {{^}}// reverse-mode derivative of myfoo08
  // CHECK08-NEXT:  // Isolation: nonisolated
  // CHECK08-NEXT:  sil private @$s3outyycfU6_7myfoo08L_yS2f_SftFTJrSSpSr : $@convention(thin) (Float, Float) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) {
  // CHECK08:         // function_ref specialized pullback of myfoo08
  // CHECK08:         %[[#T52:]] = function_ref @$s3outyycfU6_7myfoo08L_yS2f_SftFTJpSSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2fTf1nnE_n020$_AD__$s3outyycfU6_7b7L_yS2f_K26F_bb6__Pred__src_0_wrt_0_1Tf1bnnn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb6__Pred__src_0_wrt_0_1_spec, Float, Float) -> (Float, Float) // user: %[[#T53:]]
  // CHECK08:         %[[#T53]] = partial_apply [callee_guaranteed] %[[#T52]](%[[#]], %[[#]], %[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb6__Pred__src_0_wrt_0_1_spec, Float, Float) -> (Float, Float) // user: %[[#T54:]]
  // CHECK08:         %[[#T54]] = tuple (%[[#]], %[[#T53]])                          // user: %[[#T55:]]
  // CHECK08:         return %[[#T54]]                                      // id: %[[#T55]]
  // CHECK08:       } // end sil function '$s3outyycfU6_7myfoo08L_yS2f_SftFTJrSSpSr'

  // CHECK08-LABEL: {{^}}// specialized pullback of myfoo08
  // CHECK08-NEXT:  // Isolation: nonisolated
  // CHECK08-NEXT:  sil private @$s3outyycfU6_7myfoo08L_yS2f_SftFTJpSSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2fTf1nnE_n020$_AD__$s3outyycfU6_7b7L_yS2f_K26F_bb6__Pred__src_0_wrt_0_1Tf1bnnn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU6_7myfoo08L_yS2f_SftF_bb6__Pred__src_0_wrt_0_1_spec, Float, Float) -> (Float, Float) {

  // x0 > x1  && x1 < 0:  1 - x1 / x0
  // x0 > x1  && x1 >= 0: 1 + x1 / x0
  // x0 <= x1:            1
  @differentiable(reverse)
  func myfoo08(_ x0: Float, _ x1: Float) -> Float {
    let t1 = x1 + x0;
    let t2 = x0 - x1;
    let t4 = x0 > x1 ? t1 : x0;
    let t6 = 1 / x0;
    let t5 = x0 > t4 ? t2 : t4;
    let t7 = t5 * t6;
    return t7;
  }

  func myfoo08_gradient(_ x0: Float, _ x1: Float) -> (Float, Float) {
    if x0 > x1 {
      if x1 < 0 {
        return (x1 / (x0 * x0), -1 / x0)
      }
      return (-x1 / (x0 * x0), 1 / x0)
    }
    // TODO: shouldn't this be (0, 0)?
    return (1 / x0, 0)
  }

  for x0 in -10...10 {
    if x0 == 0 {
      continue
    }
    for x1 in -10...10 {
      let got = gradient(at: Float(x0), Float(x1), of: myfoo08)
      let expected = myfoo08_gradient(Float(x0), Float(x1))
      expectEqual((100000 * got.0).rounded(), (100000 * expected.0).rounded())
      expectEqual((100000 * got.1).rounded(), (100000 * expected.1).rounded())
    }
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test09,Test10") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK09 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK09-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK09-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK09-ENUM3 %s

  // CHECK09-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftF_bb1__Pred__src_0_wrt_1_spec {
  // CHECK09-ENUM1-NEXT:    case bb0(())
  // CHECK09-ENUM1-NEXT:  }

  // CHECK09-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftF_bb2__Pred__src_0_wrt_1_spec {
  // CHECK09-ENUM2-NEXT:    case bb0(())
  // CHECK09-ENUM2-NEXT:  }

  // CHECK09-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftF_bb3__Pred__src_0_wrt_1_spec_bb2_1_bb1_1 {
  // CHECK09-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftF_bb2__Pred__src_0_wrt_1_spec, (Float, Float)))
  // CHECK09-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftF_bb1__Pred__src_0_wrt_1_spec, (Float, Float)))
  // CHECK09-ENUM3-NEXT:  }

  // CHECK09-LABEL: {{^}}// reverse-mode derivative of myfoo09
  // CHECK09-NEXT:  // Isolation: nonisolated
  // CHECK09-NEXT:  sil private @$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftFTJrUSpSr : $@convention(thin) (Enum, Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK09:         // function_ref specialized pullback of myfoo09
  // CHECK09:         %[[#T30:]] = function_ref @$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftFTJpUSpSr020$_AD__$s3outyycfU7_7b11L_ySfAAyycff2_4C31L_O_SftF_bb3__Pred__src_0_wrt_1Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftF_bb3__Pred__src_0_wrt_1_spec_bb2_1_bb1_1) -> Float // user: %[[#T31:]]
  // CHECK09:         %[[#T31]] = partial_apply [callee_guaranteed] %[[#T30]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftF_bb3__Pred__src_0_wrt_1_spec_bb2_1_bb1_1) -> Float // user: %[[#T32:]]
  // CHECK09:         %[[#T32]] = tuple (%[[#]], %[[#T31]])                          // user: %[[#T33:]]
  // CHECK09:         return %[[#T32]]                                      // id: %[[#T33]]
  // CHECK09:       } // end sil function '$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftFTJrUSpSr'

  // CHECK09-LABEL: {{^}}// specialized pullback of myfoo09
  // CHECK09-NEXT:  // Isolation: nonisolated
  // CHECK09-NEXT:  sil private @$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftFTJpUSpSr020$_AD__$s3outyycfU7_7b11L_ySfAAyycff2_4C31L_O_SftF_bb3__Pred__src_0_wrt_1Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU7_7myfoo09L_ySfAAyycfU7_4EnumL_O_SftF_bb3__Pred__src_0_wrt_1_spec_bb2_1_bb1_1) -> Float {

  enum Enum {
    case a(Float)
    case b(Float)
  }

  @differentiable(reverse)
  func myfoo09(_ e: Enum, _ x: Float) -> Float {
    switch e {
    case let .a(a): return x * a
    case let .b(b): return x * b
    }
  }

  indirect enum Indirect {
    case e(Enum)
    case indirect(Indirect)
  }

  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK10 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK10-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK10-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK10-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK10-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK10-ENUM5 %s

  // CHECK10-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb1__Pred__src_0_wrt_1_spec {
  // CHECK10-ENUM1-NEXT:    case bb0(())
  // CHECK10-ENUM1-NEXT:  }

  // CHECK10-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb2__Pred__src_0_wrt_1_spec {
  // CHECK10-ENUM2-NEXT:    case bb1((predecessor: _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb1__Pred__src_0_wrt_1_spec))
  // CHECK10-ENUM2-NEXT:  }

  // CHECK10-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb3__Pred__src_0_wrt_1_spec {
  // CHECK10-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb1__Pred__src_0_wrt_1_spec))
  // CHECK10-ENUM3-NEXT:  }

  // CHECK10-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb4__Pred__src_0_wrt_1_spec {
  // CHECK10-ENUM4-NEXT:    case bb0(())
  // CHECK10-ENUM4-NEXT:  }

  // CHECK10-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb5__Pred__src_0_wrt_1_spec_bb3_2_bb2_2 {
  // CHECK10-ENUM5-NEXT:    case bb4((predecessor: _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb4__Pred__src_0_wrt_1_spec, @callee_guaranteed (Float) -> Float))
  // CHECK10-ENUM5-NEXT:    case bb3((predecessor: _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb3__Pred__src_0_wrt_1_spec, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK10-ENUM5-NEXT:    case bb2((predecessor: _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb2__Pred__src_0_wrt_1_spec, @callee_guaranteed (Float) -> Float, (Float, Float)))
  // CHECK10-ENUM5-NEXT:  }

  // CHECK10-LABEL: {{^}}// reverse-mode derivative of myfoo10
  // CHECK10-NEXT:  // Isolation: nonisolated
  // CHECK10-NEXT:  sil private @$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftFTJrUSpSr : $@convention(thin) (@guaranteed Indirect, Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK10:         // function_ref specialized pullback of myfoo10
  // CHECK10:         %[[#T73:]] = function_ref @$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftFTJpUSpSr020$_AD__$s3outyycfU7_7b11L_ySfAAyycff2_8C31L_O_SftF_bb5__Pred__src_0_wrt_1Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb5__Pred__src_0_wrt_1_spec_bb3_2_bb2_2) -> Float // user: %[[#T74:]]
  // CHECK10:         %[[#T74]] = partial_apply [callee_guaranteed] %[[#T73]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb5__Pred__src_0_wrt_1_spec_bb3_2_bb2_2) -> Float // user: %[[#T75:]]
  // CHECK10:         %[[#T75]] = tuple (%[[#]], %[[#T74]])                          // user: %[[#T76:]]
  // CHECK10:         return %[[#T75]]                                      // id: %[[#T76]]
  // CHECK10:       } // end sil function '$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftFTJrUSpSr'

  // CHECK10-LABEL: {{^}}// specialized pullback of myfoo10
  // CHECK10-NEXT:  // Isolation: nonisolated
  // CHECK10-NEXT:  sil private @$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftFTJpUSpSr020$_AD__$s3outyycfU7_7b11L_ySfAAyycff2_8C31L_O_SftF_bb5__Pred__src_0_wrt_1Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU7_7myfoo10L_ySfAAyycfU7_8IndirectL_O_SftF_bb5__Pred__src_0_wrt_1_spec_bb3_2_bb2_2) -> Float {

  @differentiable(reverse)
  func myfoo10(_ indirect: Indirect, _ x: Float) -> Float {
    switch indirect {
    case let .e(e):
      switch e {
      case .a: return x * myfoo09(e, x)
      case .b: return x * myfoo09(e, x)
      }
    case let .indirect(ind): return myfoo10(ind, x)
    }
  }

  do {
    let ind: Indirect = .e(.a(3))
    expectEqual(12, gradient(at: 2, of: { x in myfoo10(ind, x) }))
    expectEqual(12, gradient(at: 2, of: { x in myfoo10(.indirect(ind), x) }))
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test11") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM5 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM6 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM7 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM8 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM9 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK11-ENUM10 %s

  // CHECK11-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb1__Pred__src_0_wrt_0_1_spec {
  // CHECK11-ENUM1-NEXT:    case bb0(())
  // CHECK11-ENUM1-NEXT:  }

  // CHECK11-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb2__Pred__src_0_wrt_0_1_spec {
  // CHECK11-ENUM2-NEXT:    case bb1((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb1__Pred__src_0_wrt_0_1_spec))
  // CHECK11-ENUM2-NEXT:  }

  // CHECK11-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb3__Pred__src_0_wrt_0_1_spec_bb2_1 {
  // CHECK11-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb2__Pred__src_0_wrt_0_1_spec, (Float, Float)))
  // CHECK11-ENUM3-NEXT:  }

  // CHECK11-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb4__Pred__src_0_wrt_0_1_spec_bb2_1 {
  // CHECK11-ENUM4-NEXT:    case bb2((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb2__Pred__src_0_wrt_0_1_spec, (Float, Float)))
  // CHECK11-ENUM4-NEXT:  }

  // CHECK11-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb5__Pred__src_0_wrt_0_1_spec {
  // CHECK11-ENUM5-NEXT:    case bb4((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb4__Pred__src_0_wrt_0_1_spec_bb2_1))
  // CHECK11-ENUM5-NEXT:  }

  // CHECK11-ENUM6-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb6__Pred__src_0_wrt_0_1_spec {
  // CHECK11-ENUM6-NEXT:    case bb1((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb1__Pred__src_0_wrt_0_1_spec))
  // CHECK11-ENUM6-NEXT:  }

  // CHECK11-ENUM7-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb7__Pred__src_0_wrt_0_1_spec {
  // CHECK11-ENUM7-NEXT:    case bb4((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb4__Pred__src_0_wrt_0_1_spec_bb2_1))
  // CHECK11-ENUM7-NEXT:  }

  // CHECK11-ENUM8-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb8__Pred__src_0_wrt_0_1_spec {
  // CHECK11-ENUM8-NEXT:    case bb0(())
  // CHECK11-ENUM8-NEXT:  }

  // CHECK11-ENUM9-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb9__Pred__src_0_wrt_0_1_spec {
  // CHECK11-ENUM9-NEXT:    case bb7((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb7__Pred__src_0_wrt_0_1_spec))
  // CHECK11-ENUM9-NEXT:    case bb8((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb8__Pred__src_0_wrt_0_1_spec))
  // CHECK11-ENUM9-NEXT:  }

  // CHECK11-ENUM10-LABEL: {{^}}enum _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb10__Pred__src_0_wrt_0_1_spec_bb9_1_bb6_1_bb5_1_bb3_1 {
  // CHECK11-ENUM10-NEXT:    case bb9((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb9__Pred__src_0_wrt_0_1_spec, ()))
  // CHECK11-ENUM10-NEXT:    case bb6((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb6__Pred__src_0_wrt_0_1_spec, ()))
  // CHECK11-ENUM10-NEXT:    case bb5((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb5__Pred__src_0_wrt_0_1_spec, ()))
  // CHECK11-ENUM10-NEXT:    case bb3((predecessor: _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb3__Pred__src_0_wrt_0_1_spec_bb2_1, ()))
  // CHECK11-ENUM10-NEXT:  }

  // CHECK11-LABEL: {{^}}// reverse-mode derivative of myfoo11
  // CHECK11-NEXT:  // Isolation: nonisolated
  // CHECK11-NEXT:  sil private @$s3outyycfU8_7myfoo11L_yS2f_SftFTJrSSpSr : $@convention(thin) (Float, Float) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) {
  // CHECK11:         // function_ref specialized pullback of myfoo11
  // CHECK11:         %[[#T44:]] = function_ref @$s3outyycfU8_7myfoo11L_yS2f_SftFTJpSSpSr020$_AD__$s3outyycfU8_7B37L_yS2f_SftF_bb10__Pred__src_0_wrt_0_1Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb10__Pred__src_0_wrt_0_1_spec_bb9_1_bb6_1_bb5_1_bb3_1) -> (Float, Float) // user: %[[#T45:]]
  // CHECK11:         %[[#T45]] = partial_apply [callee_guaranteed] %[[#T44]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb10__Pred__src_0_wrt_0_1_spec_bb9_1_bb6_1_bb5_1_bb3_1) -> (Float, Float) // user: %[[#T46:]]
  // CHECK11:         %[[#T46]] = tuple (%[[#]], %[[#T45]])                          // user: %[[#T47:]]
  // CHECK11:         return %[[#T46]]                                      // id: %[[#T47]]
  // CHECK11:       } // end sil function '$s3outyycfU8_7myfoo11L_yS2f_SftFTJrSSpSr'

  // CHECK11-LABEL: {{^}}// specialized pullback of myfoo11
  // CHECK11-NEXT:  // Isolation: nonisolated
  // CHECK11-NEXT:  sil private @$s3outyycfU8_7myfoo11L_yS2f_SftFTJpSSpSr020$_AD__$s3outyycfU8_7B37L_yS2f_SftF_bb10__Pred__src_0_wrt_0_1Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU8_7myfoo11L_yS2f_SftF_bb10__Pred__src_0_wrt_0_1_spec_bb9_1_bb6_1_bb5_1_bb3_1) -> (Float, Float) {

  @differentiable(reverse)
  func myfoo11(_ x: Float, _ y: Float) -> Float {
    if x > 0 {
      if y > 10 {
        let z = x * y
        if z > 100 {
          return x + z
        } else if y == 20 {
          return z + z
        }
      } else {
        return x + y
      }
    }
    return -y
  }

  expectEqual((40, 8), gradient(at: 4, 20, of: myfoo11))
  expectEqual((0, -1), gradient(at: 4, 21, of: myfoo11))
  expectEqual((1, 1), gradient(at: 4, 5, of: myfoo11))
  expectEqual((0, -1), gradient(at: -3, -2, of: myfoo11))
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test12") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM5 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM6 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM7 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM8 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM9 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM10 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM11 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK12-ENUM12 %s

  // CHECK12-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb1__Pred__src_0_wrt_0_1_2_3_spec_bb0_0_1 {
  // CHECK12-ENUM1-NEXT:    case bb0(((Double, Double), ()))
  // CHECK12-ENUM1-NEXT:  }

  // CHECK12-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb2__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM2-NEXT:    case bb1((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb1__Pred__src_0_wrt_0_1_2_3_spec_bb0_0_1))
  // CHECK12-ENUM2-NEXT:  }

  // CHECK12-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb3__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb1__Pred__src_0_wrt_0_1_2_3_spec_bb0_0_1))
  // CHECK12-ENUM3-NEXT:  }

  // CHECK12-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb4__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM4-NEXT:    case bb3((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb3__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM4-NEXT:    case bb2((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb2__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM4-NEXT:  }

  // CHECK12-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb5__Pred__src_0_wrt_0_1_2_3_spec_bb0_0_1 {
  // CHECK12-ENUM5-NEXT:    case bb0(((Double, Double), ()))
  // CHECK12-ENUM5-NEXT:  }

  // CHECK12-ENUM6-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb6__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM6-NEXT:    case bb5((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb5__Pred__src_0_wrt_0_1_2_3_spec_bb0_0_1))
  // CHECK12-ENUM6-NEXT:    case bb4((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb4__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM6-NEXT:  }

  // CHECK12-ENUM7-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb7__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM7-NEXT:    case bb6((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb6__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM7-NEXT:  }

  // CHECK12-ENUM8-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb8__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM8-NEXT:    case bb7((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb7__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM8-NEXT:  }

  // CHECK12-ENUM9-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb9__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM9-NEXT:    case bb7((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb7__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM9-NEXT:  }

  // CHECK12-ENUM10-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb10__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM10-NEXT:    case bb9((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb9__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM10-NEXT:    case bb8((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb8__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM10-NEXT:  }

  // CHECK12-ENUM11-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb11__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM11-NEXT:    case bb6((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb6__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM11-NEXT:  }

  // CHECK12-ENUM12-LABEL: {{^}}enum _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb12__Pred__src_0_wrt_0_1_2_3_spec {
  // CHECK12-ENUM12-NEXT:    case bb11((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb11__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM12-NEXT:    case bb10((predecessor: _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb10__Pred__src_0_wrt_0_1_2_3_spec))
  // CHECK12-ENUM12-NEXT:  }

  // CHECK12-LABEL: {{^}}// reverse-mode derivative of myfoo12
  // CHECK12-NEXT:  // Isolation: nonisolated
  // CHECK12-NEXT:  sil private @$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtFTJrSSSSpSr : $@convention(thin) (Double, Double, Optional<Double>, Optional<Double>) -> (Double, @owned @callee_guaranteed (Double) -> (Double, Double, Optional<Double>.TangentVector, Optional<Double>.TangentVector)) {
  // CHECK12:         // function_ref specialized pullback of myfoo12
  // CHECK12:         %[[#T90:]] = function_ref @$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtFTJpSSSSpSr020$_AD__$s3outyycfU9_7b18L_2x02x12x22x3S2d_K36SgAGtF_bb12__Pred__src_0_wrt_0_1_2_3Tf1bn_n : $@convention(thin) (Double, @owned _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb12__Pred__src_0_wrt_0_1_2_3_spec) -> (Double, Double, Optional<Double>.TangentVector, Optional<Double>.TangentVector) // user: %[[#T91:]]
  // CHECK12:         %[[#T91]] = partial_apply [callee_guaranteed] %[[#T90]](%[[#]]) : $@convention(thin) (Double, @owned _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb12__Pred__src_0_wrt_0_1_2_3_spec) -> (Double, Double, Optional<Double>.TangentVector, Optional<Double>.TangentVector) // user: %[[#T92:]]
  // CHECK12:         %[[#T92]] = tuple (%[[#]], %[[#T91]])
  // CHECK12:         return %[[#T92]]
  // CHECK12:       } // end sil function '$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtFTJrSSSSpSr'

  // CHECK12-LABEL: {{^}}// specialized pullback of myfoo12
  // CHECK12-NEXT:  // Isolation: nonisolated
  // CHECK12-NEXT:  sil private @$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtFTJpSSSSpSr020$_AD__$s3outyycfU9_7b18L_2x02x12x22x3S2d_K36SgAGtF_bb12__Pred__src_0_wrt_0_1_2_3Tf1bn_n : $@convention(thin) (Double, @owned _AD__$s3outyycfU9_7myfoo12L_2x02x12x22x3S2d_S2dSgAGtF_bb12__Pred__src_0_wrt_0_1_2_3_spec) -> (Double, Double, Optional<Double>.TangentVector, Optional<Double>.TangentVector) {

  @differentiable(reverse)
  func myfoo12(x0: Double, x1: Double, x2: Optional<Double>, x3: Optional<Double>) -> Double {
    let a : Double = x1 * 42
    let b : Double = a + x0
    var c : Double = 0
    if let x2NonNil = x2 {
      if x2NonNil < b {
        c = x2NonNil
      } else {
        c = b
      }
    } else {
      c = b
    }

    var d : Double = 0
    if let x3NonNil = x3 {
      if x3NonNil < c {
        d = x3NonNil
      } else {
        d = c
      }
    } else {
      d = c
    }

    return d
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test13") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK13 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK13-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK13-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK13-ENUM3 %s

  // CHECK13-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitF_bb1__Pred__src_0_wrt_0_spec {
  // CHECK13-ENUM1-NEXT:    case bb0(())
  // CHECK13-ENUM1-NEXT:  }

  // CHECK13-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitF_bb2__Pred__src_0_wrt_0_spec {
  // CHECK13-ENUM2-NEXT:    case bb0(())
  // CHECK13-ENUM2-NEXT:  }

  // CHECK13-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitF_bb3__Pred__src_0_wrt_0_spec_bb1_1 {
  // CHECK13-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitF_bb2__Pred__src_0_wrt_0_spec))
  // CHECK13-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitF_bb1__Pred__src_0_wrt_0_spec, ()))
  // CHECK13-ENUM3-NEXT:  }

  // CHECK13-LABEL: {{^}}// reverse-mode derivative of myfoo13
  // CHECK13-NEXT:  // Isolation: nonisolated
  // CHECK13-NEXT:  sil private [signature_optimized_thunk] [heuristic_always_inline] @$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitFTJrSUpSr : $@convention(thin) (X, Int) -> (Float, @owned @callee_guaranteed (Float) -> X.TangentVector) {
  // CHECK13:         // function_ref specialized pullback of myfoo13
  // CHECK13:         %[[#T21:]] = function_ref @$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitFTJpSUpSr021$_AD__$s3outyycfU10_7b14L_1x1ySfAAyycfE34_1XL_V_SitF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitF_bb3__Pred__src_0_wrt_0_spec_bb1_1) -> X.TangentVector // user: %[[#T22:]]
  // CHECK13:         %[[#T22]] = partial_apply [callee_guaranteed] %[[#T21]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitF_bb3__Pred__src_0_wrt_0_spec_bb1_1) -> X.TangentVector // user: %[[#T23:]]
  // CHECK13:         %[[#T23]] = tuple (%[[#]], %[[#T22]])                          // user: %[[#T24:]]
  // CHECK13:         return %[[#T23]]                                      // id: %[[#T24]]
  // CHECK13:       } // end sil function '$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitFTJrSUpSr'

  // CHECK13-LABEL: {{^}}// specialized pullback of myfoo13
  // CHECK13-NEXT:  // Isolation: nonisolated
  // CHECK13-NEXT:  sil private @$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitFTJpSUpSr021$_AD__$s3outyycfU10_7b14L_1x1ySfAAyycfE34_1XL_V_SitF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU10_7myfoo13L_1x1ySfAAyycfU10_1XL_V_SitF_bb3__Pred__src_0_wrt_0_spec_bb1_1) -> X.TangentVector {

  struct X: Differentiable {
    var a: Float
    var b: Double
  }

  @differentiable(reverse)
  func callee_myfoo13(x: X) -> (Float, Double) {
    (x.a, x.b)
  }

  @differentiable(reverse)
  func myfoo13(x: X, y: Int) -> Float {
    if y < 0 {
      return callee_myfoo13(x: x).0
    } else {
      return 37
    }
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test14,Test15") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK14 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK14-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK14-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK14-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK14-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK14-ENUM5 %s

  // CHECK14-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb1__Pred__src_0_1_wrt_0_1_spec {
  // CHECK14-ENUM1-NEXT:    case bb0(())
  // CHECK14-ENUM1-NEXT:  }

  // CHECK14-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb2__Pred__src_0_1_wrt_0_1_spec {
  // CHECK14-ENUM2-NEXT:    case bb1((predecessor: _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb1__Pred__src_0_1_wrt_0_1_spec))
  // CHECK14-ENUM2-NEXT:  }

  // CHECK14-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb3__Pred__src_0_1_wrt_0_1_spec {
  // CHECK14-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb1__Pred__src_0_1_wrt_0_1_spec))
  // CHECK14-ENUM3-NEXT:  }

  // CHECK14-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb4__Pred__src_0_1_wrt_0_1_spec {
  // CHECK14-ENUM4-NEXT:    case bb0(())
  // CHECK14-ENUM4-NEXT:  }

  // CHECK14-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb5__Pred__src_0_1_wrt_0_1_spec_bb4_1_2_bb3_1_2_bb2_1_2 {
  // CHECK14-ENUM5-NEXT:    case bb4((predecessor: _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb4__Pred__src_0_1_wrt_0_1_spec, (Float, Float), ()))
  // CHECK14-ENUM5-NEXT:    case bb3((predecessor: _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb3__Pred__src_0_1_wrt_0_1_spec, (Float, Float), ()))
  // CHECK14-ENUM5-NEXT:    case bb2((predecessor: _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb2__Pred__src_0_1_wrt_0_1_spec, (), ()))
  // CHECK14-ENUM5-NEXT:  }

  // CHECK14-LABEL: {{^}}// reverse-mode derivative of myfoo14
  // CHECK14-NEXT:  // Isolation: nonisolated
  // CHECK14-NEXT:  sil private @$s3outyycfU11_7myfoo14L_ySf_SftSf_SftFTJrSSpSSr : $@convention(thin) (Float, Float) -> (Float, Float, @owned @callee_guaranteed (Float, Float) -> (Float, Float)) {
  // CHECK14:         // function_ref specialized pullback of myfoo14
  // CHECK14:         %[[#T53:]] = function_ref @$s3outyycfU11_7myfoo14L_ySf_SftSf_SftFTJpSSpSSr021$_AD__$s3outyycfU11_7b9L_ySf_Sftf1_G28F_bb5__Pred__src_0_1_wrt_0_1Tf1bnn_n : $@convention(thin) (Float, Float, @owned _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb5__Pred__src_0_1_wrt_0_1_spec_bb4_1_2_bb3_1_2_bb2_1_2) -> (Float, Float) // user: %[[#T54:]]
  // CHECK14:         %[[#T54]] = partial_apply [callee_guaranteed] %[[#T53]](%[[#]]) : $@convention(thin) (Float, Float, @owned _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb5__Pred__src_0_1_wrt_0_1_spec_bb4_1_2_bb3_1_2_bb2_1_2) -> (Float, Float) // user: %[[#T55:]]
  // CHECK14:         %[[#T55]] = tuple (%[[#]], %[[#]], %[[#T54]])                     // user: %[[#T56:]]
  // CHECK14:         return %[[#T55]]                                      // id: %[[#T56]]
  // CHECK14:       } // end sil function '$s3outyycfU11_7myfoo14L_ySf_SftSf_SftFTJrSSpSSr'

  // CHECK14-LABEL: {{^}}// specialized pullback of myfoo14
  // CHECK14-NEXT:  // Isolation: nonisolated
  // CHECK14-NEXT:  sil private @$s3outyycfU11_7myfoo14L_ySf_SftSf_SftFTJpSSpSSr021$_AD__$s3outyycfU11_7b9L_ySf_Sftf1_G28F_bb5__Pred__src_0_1_wrt_0_1Tf1bnn_n : $@convention(thin) (Float, Float, @owned _AD__$s3outyycfU11_7myfoo14L_ySf_SftSf_SftF_bb5__Pred__src_0_1_wrt_0_1_spec_bb4_1_2_bb3_1_2_bb2_1_2) -> (Float, Float) {

  @differentiable(reverse)
  func myfoo14(_ x : Float, _ y : Float) -> (Float, Float) {
    if x > 0 {
      if y > 2 {
        return (y - 37, x + 5)
      } else {
        return (y * 42, x - 3)
      }
    }
    return (7 * x, 8 - y)
  }

  struct FloatPair : Differentiable {
    var first, second: Float
    init(_ first: Float, _ second: Float) {
      self.first = first
      self.second = second
    }
  }

  struct Pair<T : Differentiable, U : Differentiable> : Differentiable {
    var first: T
    var second: U
    init(_ first: T, _ second: U) {
      self.first = first
      self.second = second
    }
  }

  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK15 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK15-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK15-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK15-ENUM3 %s

  // CHECK15-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU11_7myfoo15L_yS2fF_bb1__Pred__src_0_wrt_0_spec_bb0_0_1_2_3 {
  // CHECK15-ENUM1-NEXT:    case bb0(((), (), (), (_: @callee_guaranteed (@in_guaranteed Pair<FloatPair, Float>.TangentVector) -> (@out FloatPair.TangentVector, @out Float))))
  // CHECK15-ENUM1-NEXT:  }

  // CHECK15-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU11_7myfoo15L_yS2fF_bb2__Pred__src_0_wrt_0_spec_bb0_0_1_2_3 {
  // CHECK15-ENUM2-NEXT:    case bb0(((), (), (), (_: @callee_guaranteed (@in_guaranteed Pair<FloatPair, Float>.TangentVector) -> (@out FloatPair.TangentVector, @out Float))))
  // CHECK15-ENUM2-NEXT:  }

  // CHECK15-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU11_7myfoo15L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_3_4_bb1_1_2_3 {
  // CHECK15-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU11_7myfoo15L_yS2fF_bb2__Pred__src_0_wrt_0_spec_bb0_0_1_2_3, (), (), (), (_: @callee_guaranteed (@in_guaranteed Pair<FloatPair, Float>.TangentVector) -> (@out FloatPair.TangentVector, @out Float))))
  // CHECK15-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU11_7myfoo15L_yS2fF_bb1__Pred__src_0_wrt_0_spec_bb0_0_1_2_3, (), (), ()))
  // CHECK15-ENUM3-NEXT:  }

  // CHECK15-LABEL: {{^}}// reverse-mode derivative of myfoo15
  // CHECK15-NEXT:  // Isolation: nonisolated
  // CHECK15-NEXT:  sil private @$s3outyycfU11_7myfoo15L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK15:         // function_ref specialized pullback of myfoo15
  // CHECK15:         %[[#T59:]] = function_ref @$s3outyycfU11_7myfoo15L_yS2fFTJpSpSr067$sSf16_DifferentiationE7_vjpAdd3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_0cd1_e4E12_g16Subtract3lhs3rhsi1_j1_klj1_km1_kN2U_ACTf1nnccc_n021$_AD__$s3outyycfU11_7B30L_yS2fF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU11_7myfoo15L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_3_4_bb1_1_2_3) -> Float // user: %[[#T60:]]
  // CHECK15:         %[[#T60]] = partial_apply [callee_guaranteed] %[[#T59]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU11_7myfoo15L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_3_4_bb1_1_2_3) -> Float // user: %[[#T61:]]
  // CHECK15:         %[[#T61]] = tuple (%[[#]], %[[#T60]])
  // CHECK15:         return %[[#T61]]
  // CHECK15:       } // end sil function '$s3outyycfU11_7myfoo15L_yS2fFTJrSpSr'

  // CHECK15-LABEL: {{^}}// specialized pullback of myfoo15
  // CHECK15-NEXT:  // Isolation: nonisolated
  // CHECK15-NEXT:  sil private @$s3outyycfU11_7myfoo15L_yS2fFTJpSpSr067$sSf16_DifferentiationE7_vjpAdd3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_0cd1_e4E12_g16Subtract3lhs3rhsi1_j1_klj1_km1_kN2U_ACTf1nnccc_n021$_AD__$s3outyycfU11_7B30L_yS2fF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU11_7myfoo15L_yS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_2_3_4_bb1_1_2_3) -> Float {

  @differentiable(reverse)
  func myfoo15(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y = FloatPair(x + x, x - x)
    var z = Pair(y, x)
    if x > 0 {
      var w = FloatPair(x, x)
      y.first = w.second
      y.second = w.first
      z.first.first = z.first.first - y.first
      z.first.second = z.first.second + y.first
    } else {
      z = Pair(FloatPair(y.first - x, y.second + x), x)
    }
    return y.first + y.second - z.first.first + z.first.second
  }

  expectEqual((8, 2), valueWithGradient(at: 4, of: myfoo15))
  expectEqual((-20, 2), valueWithGradient(at: -10, of: myfoo15))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, of: myfoo15))
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test16") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK16 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK16-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK16-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK16-ENUM3 %s

  // CHECK16-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStF_bb1__Pred__src_0_wrt_0_spec {
  // CHECK16-ENUM1-NEXT:    case bb0(())
  // CHECK16-ENUM1-NEXT:  }

  // CHECK16-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStF_bb2__Pred__src_0_wrt_0_spec {
  // CHECK16-ENUM2-NEXT:    case bb0(())
  // CHECK16-ENUM2-NEXT:  }

  // CHECK16-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0_spec_bb1_1 {
  // CHECK16-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStF_bb2__Pred__src_0_wrt_0_spec))
  // CHECK16-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStF_bb1__Pred__src_0_wrt_0_spec, (_: @callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>)))
  // CHECK16-ENUM3-NEXT:  }

  // CHECK16-LABEL: {{^}}// reverse-mode derivative of myfoo16
  // CHECK16-NEXT:  // Isolation: nonisolated
  // CHECK16-NEXT:  sil private @$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStFTJrSUpSr : $@convention(thin) (@guaranteed Dictionary<String, Double>, @guaranteed String) -> (Optional<Double>, @owned @callee_guaranteed (Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>) {
  // CHECK16:         // function_ref specialized pullback of myfoo16
  // CHECK16:         %[[#T35:]] = function_ref @$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStFTJpSUpSr021$_AD__$s3outyycfU12_7B50L_4from2atSdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Optional<Double>.TangentVector, @owned _AD__$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0_spec_bb1_1) -> @owned Dictionary<String, Double> // user: %[[#T36:]]
  // CHECK16:         %[[#T36]] = partial_apply [callee_guaranteed] %[[#T35]](%[[#]]) : $@convention(thin) (Optional<Double>.TangentVector, @owned _AD__$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0_spec_bb1_1) -> @owned Dictionary<String, Double> // user: %[[#T37:]]
  // CHECK16:         %[[#T37]] = tuple (%[[#]], %[[#T36]])                          // user: %[[#T38:]]
  // CHECK16:         return %[[#T37]]                                      // id: %[[#T38]]
  // CHECK16:       } // end sil function '$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStFTJrSUpSr'

  // CHECK16-LABEL: {{^}}// specialized pullback of myfoo16
  // CHECK16-NEXT:  // Isolation: nonisolated
  // CHECK16-NEXT:  sil private @$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStFTJpSUpSr021$_AD__$s3outyycfU12_7B50L_4from2atSdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0Tf1bn_n : $@convention(thin) (Optional<Double>.TangentVector, @owned _AD__$s3outyycfU12_7myfoo16L_4from2atSdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0_spec_bb1_1) -> @owned Dictionary<String, Double> {

  func myfoo16(from newValues: [String: Double], at key: String) -> Double? {
    if newValues.keys.contains(key) {
      return newValues[key]
    }
    return nil
  }

  @differentiable(reverse)
  func caller_myfoo16(newValues: [String: Double]) -> Double {
    return myfoo16(from: newValues, at: "s1")!
  }

  expectEqual(pullback(at: ["s1": 1.0], of: caller_myfoo16)(2), ["s1" : 2.0])
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test19") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM5 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM6 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM7 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM8 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK19-ENUM9 %s

  // CHECK19-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb1__Pred__src_0_1_wrt_0_1_spec {
  // CHECK19-ENUM1-NEXT:    case bb0(())
  // CHECK19-ENUM1-NEXT:  }

  // CHECK19-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb2__Pred__src_0_1_wrt_0_1_spec {
  // CHECK19-ENUM2-NEXT:    case bb1((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb1__Pred__src_0_1_wrt_0_1_spec))
  // CHECK19-ENUM2-NEXT:  }

  // CHECK19-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb3__Pred__src_0_1_wrt_0_1_spec {
  // CHECK19-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb1__Pred__src_0_1_wrt_0_1_spec))
  // CHECK19-ENUM3-NEXT:  }

  // CHECK19-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb4__Pred__src_0_1_wrt_0_1_spec_bb2_1 {
  // CHECK19-ENUM4-NEXT:    case bb3((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb3__Pred__src_0_1_wrt_0_1_spec))
  // CHECK19-ENUM4-NEXT:    case bb2((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb2__Pred__src_0_1_wrt_0_1_spec, (Float, Float, Float)))
  // CHECK19-ENUM4-NEXT:  }

  // CHECK19-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb5__Pred__src_0_1_wrt_0_1_spec {
  // CHECK19-ENUM5-NEXT:    case bb0(())
  // CHECK19-ENUM5-NEXT:  }

  // CHECK19-ENUM6-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb6__Pred__src_0_1_wrt_0_1_spec {
  // CHECK19-ENUM6-NEXT:    case bb5((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb5__Pred__src_0_1_wrt_0_1_spec))
  // CHECK19-ENUM6-NEXT:  }

  // CHECK19-ENUM7-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb7__Pred__src_0_1_wrt_0_1_spec {
  // CHECK19-ENUM7-NEXT:    case bb5((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb5__Pred__src_0_1_wrt_0_1_spec))
  // CHECK19-ENUM7-NEXT:  }

  // CHECK19-ENUM8-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb8__Pred__src_0_1_wrt_0_1_spec_bb6_1 {
  // CHECK19-ENUM8-NEXT:    case bb7((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb7__Pred__src_0_1_wrt_0_1_spec))
  // CHECK19-ENUM8-NEXT:    case bb6((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb6__Pred__src_0_1_wrt_0_1_spec, (Float, Float, Float)))
  // CHECK19-ENUM8-NEXT:  }

  // CHECK19-ENUM9-LABEL: {{^}}enum _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb9__Pred__src_0_1_wrt_0_1_spec_bb8_1_bb4_1 {
  // CHECK19-ENUM9-NEXT:    case bb8((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb8__Pred__src_0_1_wrt_0_1_spec_bb6_1, (Float, Float)))
  // CHECK19-ENUM9-NEXT:    case bb4((predecessor: _AD__$s3outyycfU13_7myfoo17L_ySf_SftSf_SftF_bb4__Pred__src_0_1_wrt_0_1_spec_bb2_1, (Float, Float)))
  // CHECK19-ENUM9-NEXT:  }

  // CHECK19-LABEL: {{^}}// reverse-mode derivative of myfoo17
  // CHECK19-NEXT:  // Isolation: nonisolated
  // CHECK19-NEXT:  sil private @$s3outyycfU13_7myfoo17L_ySf_SftSf_SftFTJrSSpSSr : $@convention(thin) (Float, Float) -> (Float, Float, @owned @callee_guaranteed (Float, Float) -> (Float, Float)) {
  // CHECK19:         // function_ref specialized pullback of myfoo17
  // CHECK19:         %[[#T45:]] = function_ref @$s3outyycfU13_7myfoo17L_ySf_SftSf_SftFTJpSSpSSr021$_AD__$s3outyycfU13_7b9L_ySf_Sftf1_G28F_bb9__Pred__src_0_1_wrt_0_1Tf1bnn_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float) // user: %[[#T46:]]
  // CHECK19:         %[[#T46]] = thin_to_thick_function %[[#T45]] to $@callee_guaranteed (Float, Float) -> (Float, Float) // user: %[[#T47:]]
  // CHECK19:         %[[#T47]] = tuple (%[[#]], %[[#]], %[[#T46]])
  // CHECK19:         return %[[#T47]]
  // CHECK19:       } // end sil function '$s3outyycfU13_7myfoo17L_ySf_SftSf_SftFTJrSSpSSr'

  // CHECK19-LABEL: {{^}}// specialized pullback of myfoo17
  // CHECK19-NEXT:  // Isolation: nonisolated
  // CHECK19-NEXT:  sil private [signature_optimized_thunk] [heuristic_always_inline] @$s3outyycfU13_7myfoo17L_ySf_SftSf_SftFTJpSSpSSr021$_AD__$s3outyycfU13_7b9L_ySf_Sftf1_G28F_bb9__Pred__src_0_1_wrt_0_1Tf1bnn_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float) {

  @differentiable(reverse)
  func myfoo17(_ t0 : Float, _ t1 : Float) -> (Float, Float) {
    if t0 < 0 {
      var t21 : Float = 0
      let t14 : Float = 2
      if 0 < t1 {
        t21 = pow(t1, t14)
      } else {
        t21 = Float.nan
      }

      let t10 : Float = 3
      let t26 : Float = t21
      let t34 : Float = 1
      let t28 : Float = t10 * t26
      return (t28, t34)
    } else {
      let t43 : Float = 4
      let t46 : Float = 5
      var t54 : Float = 0
      if 0 < t1 {
        t54 = pow(t1, t46)
      } else {
        t54 = Float.nan
      }

      let t59 : Float = t54
      let t61 : Float = t43 * t59
      let t67 : Float = 6

      return (t61, t67)
    }
  }
}

AutoDiffClosureSpecializationTests.testWithLeakChecking("Test20") {
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK20 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK20-ENUM1 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK20-ENUM2 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK20-ENUM3 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK20-ENUM4 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK20-ENUM5 %s
  // RUN: cat %t/out.sil | %FileCheck --check-prefix=CHECK20-ENUM6 %s

  // CHECK20-ENUM1-LABEL: {{^}}enum _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb1__Pred__src_0_wrt_0_l_spec<τ_0_0> {
  // CHECK20-ENUM1-NEXT:    case bb0(())
  // CHECK20-ENUM1-NEXT:  }

  // CHECK20-ENUM2-LABEL: {{^}}enum _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb2__Pred__src_0_wrt_0_l_spec<τ_0_0> {
  // CHECK20-ENUM2-NEXT:    case bb0(())
  // CHECK20-ENUM2-NEXT:  }

  // CHECK20-ENUM3-LABEL: {{^}}enum _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec<τ_0_0> {
  // CHECK20-ENUM3-NEXT:    case bb2((predecessor: _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb2__Pred__src_0_wrt_0_l_spec<τ_0_0>))
  // CHECK20-ENUM3-NEXT:    case bb1((predecessor: _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb1__Pred__src_0_wrt_0_l_spec<τ_0_0>))
  // CHECK20-ENUM3-NEXT:  }

  // CHECK20-ENUM4-LABEL: {{^}}enum _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb4__Pred__src_0_wrt_0_l_spec<τ_0_0> {
  // CHECK20-ENUM4-NEXT:    case bb3((predecessor: _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec<τ_0_0>))
  // CHECK20-ENUM4-NEXT:  }

  // CHECK20-ENUM5-LABEL: {{^}}enum _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb5__Pred__src_0_wrt_0_l_spec<τ_0_0> {
  // CHECK20-ENUM5-NEXT:    case bb3((predecessor: _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec<τ_0_0>))
  // CHECK20-ENUM5-NEXT:  }

  // CHECK20-ENUM6-LABEL: {{^}}enum _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb6__Pred__src_0_wrt_0_l_spec_bb5_1_bb4_1<τ_0_0> {
  // CHECK20-ENUM6-NEXT:    case bb5((predecessor: _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb5__Pred__src_0_wrt_0_l_spec<τ_0_0>, (Float, Float)))
  // CHECK20-ENUM6-NEXT:    case bb4((predecessor: _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb4__Pred__src_0_wrt_0_l_spec<τ_0_0>, ()))
  // CHECK20-ENUM6-NEXT:  }

  // CHECK20-LABEL: {{^}}// reverse-mode derivative of myfoo18
  // CHECK20-NEXT:  // Isolation: nonisolated
  // CHECK20-NEXT:  sil private @$s3outyycfU14_7myfoo18L_yS2f_xmtlFlTJrSUpSr : $@convention(thin) <τ_0_0> (Float, @thick τ_0_0.Type) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK20:         // function_ref specialized pullback of myfoo18
  // CHECK20:         %[[#T32:]] = function_ref @$s3outyycfU14_7myfoo18L_yS2f_xmtlFlTJpSUpSr0083$_AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb6__Pred__src_0_wrt_0_l_0_0_ECBcpDCACnjeaTf1bn_n : $@convention(thin) <τ_0_0> (Float, @owned _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb6__Pred__src_0_wrt_0_l_spec_bb5_1_bb4_1<τ_0_0>) -> Float // user: %[[#T33:]]
  // CHECK20:         %[[#T33]] = partial_apply [callee_guaranteed] %[[#T32]]<τ_0_0>(%[[#]]) : $@convention(thin) <τ_0_0> (Float, @owned _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb6__Pred__src_0_wrt_0_l_spec_bb5_1_bb4_1<τ_0_0>) -> Float // user: %[[#T34:]]
  // CHECK20:         %[[#T34]] = tuple (%[[#]], %[[#T33]])                          // user: %[[#T35:]]
  // CHECK20:         return %[[#T34]]                                      // id: %[[#T35]]
  // CHECK20:       } // end sil function '$s3outyycfU14_7myfoo18L_yS2f_xmtlFlTJrSUpSr'

  // CHECK20-LABEL: {{^}}// specialized pullback of myfoo18
  // CHECK20-NEXT:  // Isolation: nonisolated
  // CHECK20-NEXT:  sil private @$s3outyycfU14_7myfoo18L_yS2f_xmtlFlTJpSUpSr0083$_AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb6__Pred__src_0_wrt_0_l_0_0_ECBcpDCACnjeaTf1bn_n : $@convention(thin) <τ_0_0> (Float, @owned _AD__$s3outyycfU14_7myfoo18L_yS2f_xmtlF_bb6__Pred__src_0_wrt_0_l_spec_bb5_1_bb4_1<τ_0_0>) -> Float {

  // checked_cast_br
  func myfoo18<T>(_ x: Float, _ metatype: T.Type) -> Float {
    if metatype is Int.Type {
      return x + x
    }
    return x * x
  }
  expectEqual((6, 2), valueWithGradient(at: 3, of: { myfoo18($0, Int.self) }))
  expectEqual((9, 6), valueWithGradient(at: 3, of: { myfoo18($0, Float.self) }))
}

runAllTests()
