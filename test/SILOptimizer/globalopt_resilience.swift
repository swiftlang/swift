// RUN: %target-swift-frontend  -O -module-name=test -enable-library-evolution -emit-sil -primary-file %s | %FileCheck %s

// Check if GlobalOpt generates the optimal getter for a static property with a resilient type.
// The (resilient) getter should just return the literal (and not lazily initialize a global variable).

// CHECK-LABEL: sil @$s4test15ResilientStructV9staticValACvgZ :
// CHECK:     bb0(%0 : $*ResilientStruct{{.*}}):
// CHECK:       [[L:%[0-9]+]] = integer_literal {{.*}}, 27
// CHECK:       [[I:%[0-9]+]] = struct $Int ([[L]] : {{.*}})
// CHECK:       [[S:%[0-9]+]] = struct $ResilientStruct ([[I]] : $Int)
// CHECK:       store [[S]] to %0
// CHECK:       return

public struct ResilientStruct {
  var rawValue: Int

  public static let staticVal = ResilientStruct(rawValue: 27)

  @_optimize(none)
  public func method() {}
}

public func cannotConvertToValueUse() {
  // We can't optimize this because the method takes the resilient value as
  // @in_guaranteed
  ResilientStruct.staticVal.method()
}

// CHECK-LABEL: sil @$s4test23cannotConvertToValueUseyyF : $@convention(thin) () -> ()
// CHECK: [[ADDR:%.*]] = global_addr @$s4test15ResilientStructV9staticValACvpZ : $*ResilientStruct
// CHECK: [[METHOD:%.*]] = function_ref @$s4test15ResilientStructV6methodyyF : $@convention(method) (@in_guaranteed ResilientStruct) -> ()
// CHECK: apply [[METHOD]]([[ADDR]]) : $@convention(method) (@in_guaranteed ResilientStruct) -> ()
// CHECK: [[RESULT:%.*]] = tuple ()
// CHECK: return [[RESULT]] : $()

internal struct WrapperStruct {
  var inner: ResilientStruct = .staticVal

  init() {}
}

func returnWrapperStruct() -> WrapperStruct {
  return WrapperStruct()
}

// CHECK-LABEL: sil hidden @$s4test19returnWrapperStructAA0cD0VyF : $@convention(thin) () -> @out WrapperStruct
// CHECK: bb0(%0 : $*WrapperStruct):
// CHECK: [[INT:%.*]] = integer_literal $Builtin.Int{{32|64}}, 27
// CHECK: [[S1:%.*]] = struct $Int ([[INT]] : $Builtin.Int{{32|64}})
// CHECK: [[S2:%.*]] = struct $ResilientStruct ([[S1]] : $Int)
// CHECK: [[S3:%.*]] = struct $WrapperStruct ([[S2]] : $ResilientStruct)
// CHECK: store [[S3]] to %0 : $*WrapperStruct
// CHECK: [[RESULT:%.*]] = tuple ()
// CHECK: return [[RESULT]] : $()
