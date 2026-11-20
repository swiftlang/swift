// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name CS %s | %FileCheck %s

public protocol Observer {
  func observe(_ result: Float, recompute: () -> Float)
}

// CHECK:       {{^}}// specialized applyAndObserve(_:_:observer:)
// CHECK-LABEL: {{^}}sil shared [noinline] @$s2CS15applyAndObserve__8observerS2f_S2fXEAA8Observer_ptF018$s2CS9transform__8e9S2f_SfAA8F12_ptFS2fXEfU_SfTf1ncn_n : $@convention(thin) (Float, @in_guaranteed any Observer, Float) -> Float {
// CHECK:       bb0(%0 : $Float, %1 : $*any Observer, %2 : $Float):
// CHECK:         %[[#OBS_LHS:]] = struct_extract %0, #Float._value
// CHECK-NEXT:    %[[#OBS_RHS:]] = struct_extract %2, #Float._value
// CHECK-NEXT:    %[[#OBS_PRODUCT:]] = builtin "fmul_FPIEEE32"(%[[#OBS_LHS]], %[[#OBS_RHS]]) : $Builtin.FPIEEE32
// CHECK-NEXT:    %[[#OBS_RESULT:]] = struct $Float (%[[#OBS_PRODUCT]])
// CHECK:         return %[[#OBS_RESULT]]
// CHECK-NEXT:  {{^}}} // end sil function
@inline(never)
func applyAndObserve(_ value: Float, _ body: (Float) -> Float, observer: any Observer) -> Float {
  let result = body(value)
  observer.observe(result, recompute: { body(value) })
  return result
}

// CHECK:       {{^}}// transform(_:_:observer:)
// CHECK-LABEL: {{^}}sil @$s2CS9transform__8observerS2f_SfAA8Observer_ptF :
// CHECK:       bb0(%0 : $Float, %1 : $Float, %2 : $*any Observer):
// CHECK-NOT:     partial_apply
// CHECK:         // function_ref specialized applyAndObserve(_:_:observer:)
// CHECK-NEXT:    %[[#OBS_ARG_USE:]] = function_ref @$s2CS15applyAndObserve__8observerS2f_S2fXEAA8Observer_ptF018$s2CS9transform__8e9S2f_SfAA8F12_ptFS2fXEfU_SfTf1ncn_n : $@convention(thin) (Float, @in_guaranteed any Observer, Float) -> Float
// CHECK-NEXT:    %[[#OBS_ARG_RESULT:]] = apply %[[#OBS_ARG_USE]](%0, %2, %1) : $@convention(thin) (Float, @in_guaranteed any Observer, Float) -> Float
// CHECK-NEXT:    return %[[#OBS_ARG_RESULT]]
// CHECK-NEXT:  {{^}}} // end sil function '$s2CS9transform__8observerS2f_SfAA8Observer_ptF'
public func transform(_ value: Float, _ factor: Float, observer: any Observer) -> Float {
  applyAndObserve(value, { $0 * factor }, observer: observer)
}

public protocol Visitor {
  func visit<T>(_ value: T, transform: (T) -> T)
}

// CHECK:       {{^}}// specialized applyAndVisit(_:_:_:)
// CHECK-LABEL: {{^}}sil shared [noinline] @$s2CS13applyAndVisityS2f_S2fXEAA7Visitor_ptF030$s2CS13updateVisitedyS2f_SfAA7E12_ptFS2fXEfU_SfTf1ncn_n : $@convention(thin) (Float, @in_guaranteed any Visitor, Float) -> Float {
// CHECK:       bb0(%0 : $Float, %1 : $*any Visitor, %2 : $Float):
// CHECK:         %[[#VIS_LHS:]] = struct_extract %0, #Float._value
// CHECK-NEXT:    %[[#VIS_RHS:]] = struct_extract %2, #Float._value
// CHECK-NEXT:    %[[#VIS_PRODUCT:]] = builtin "fmul_FPIEEE32"(%[[#VIS_LHS]], %[[#VIS_RHS]]) : $Builtin.FPIEEE32
// CHECK-NEXT:    %[[#VIS_RESULT:]] = struct $Float (%[[#VIS_PRODUCT]])
// CHECK:         return %[[#VIS_RESULT]]
// CHECK-NEXT:  {{^}}} // end sil function
@inline(never)
func applyAndVisit(_ value: Float, _ transform: (Float) -> Float, _ visitor: any Visitor) -> Float {
  let result = transform(value)
  visitor.visit(value, transform: transform)
  return result
}

// CHECK:       {{^}}// updateVisited(_:_:_:)
// CHECK-LABEL: {{^}}sil @$s2CS13updateVisitedyS2f_SfAA7Visitor_ptF :
// CHECK:       bb0(%0 : $Float, %1 : $Float, %2 : $*any Visitor):
// CHECK-NOT:     partial_apply
// CHECK:         // function_ref specialized applyAndVisit(_:_:_:)
// CHECK-NEXT:    %[[#VIS_ARG_USE:]] = function_ref @$s2CS13applyAndVisityS2f_S2fXEAA7Visitor_ptF030$s2CS13updateVisitedyS2f_SfAA7E12_ptFS2fXEfU_SfTf1ncn_n : $@convention(thin) (Float, @in_guaranteed any Visitor, Float) -> Float
// CHECK-NEXT:    %[[#VIS_ARG_RESULT:]] = apply %[[#VIS_ARG_USE]](%0, %2, %1) : $@convention(thin) (Float, @in_guaranteed any Visitor, Float) -> Float
// CHECK-NEXT:    return %[[#VIS_ARG_RESULT]]
// CHECK-NEXT:  {{^}}} // end sil function '$s2CS13updateVisitedyS2f_SfAA7Visitor_ptF'
public func updateVisited(_ value: Float, _ factor: Float, _ visitor: any Visitor) -> Float {
  applyAndVisit(value, { $0 * factor }, visitor)
}
