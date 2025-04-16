// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

@_silgen_name("takeGenericKp")
func takeGenericKp<T: Collection>(_: KeyPath<T, Int>)

// CHECK-LABEL: sil @$s11cse_keypath7genericyyxmSlRzlF : $@convention(thin) <T where T : Collection> (@thick T.Type) -> () {
// CHECK:         [[KP:%.*]] = keypath $KeyPath<T, Int>
// CHECK:         [[FN_REF:%.*]] = function_ref @takeGenericKp
// CHECK-NEXT:    [[APPLY_0:%.*]] = apply [[FN_REF]]<T>([[KP]])
// CHECK-NEXT:    [[APPLY_1:%.*]] = apply [[FN_REF]]<T>([[KP]])
// CHECK-NEXT:    strong_release [[KP]]
// CHECK-LABEL: } // end sil function '$s11cse_keypath7genericyyxmSlRzlF'
public func generic<T: Collection>(_: T.Type) {
  takeGenericKp(\T.count)
  takeGenericKp(\T.count)
}

// CHECK-LABEL: sil @$s11cse_keypath9concrete1yyF : $@convention(thin) () -> () {
// CHECK:         [[KP:%.*]] = keypath $KeyPath<Array<Int>, Int>
// CHECK:         [[FN_REF:%.*]] = function_ref @takeGenericKp
// CHECK-NEXT:    [[APPLY_0:%.*]] = apply [[FN_REF]]<Array<Int>>([[KP]])
// CHECK-NEXT:    [[APPLY_1:%.*]] = apply [[FN_REF]]<Array<Int>>([[KP]])
// CHECK-NEXT:    [[APPLY_2:%.*]] = apply [[FN_REF]]<Array<Int>>([[KP]])
// CHECK-NEXT:    [[APPLY_3:%.*]] = apply [[FN_REF]]<Array<Int>>([[KP]])
// CHECK-NEXT:    strong_release [[KP]]
// CHECK-LABEL: } // end sil function '$s11cse_keypath9concrete1yyF'
public func concrete1() {
  generic([Int].self)
  generic([Int].self)
}

// CHECK-LABEL: sil @$s11cse_keypath9concrete2yyF : $@convention(thin) () -> () {
// CHECK:         [[KP_0:%.*]] = keypath $KeyPath<Array<Int>, Int>
// CHECK:         [[FN_REF:%.*]] = function_ref @takeGenericKp
// CHECK-NEXT:    [[APPLY_0:%.*]] = apply [[FN_REF]]<Array<Int>>([[KP_0]])
// CHECK-NEXT:    [[APPLY_1:%.*]] = apply [[FN_REF]]<Array<Int>>([[KP_0]])
// CHECK-NEXT:    strong_release [[KP_0]]
// CHECK-NEXT:    [[KP_1:%.*]] = keypath $KeyPath<Array<String>, Int>
// CHECK-NEXT:    [[APPLY_2:%.*]] = apply [[FN_REF]]<Array<String>>([[KP_1]])
// CHECK-NEXT:    [[APPLY_3:%.*]] = apply [[FN_REF]]<Array<String>>([[KP_1]])
// CHECK-NEXT:    strong_release [[KP_1]]
// CHECK-LABEL: } // end sil function '$s11cse_keypath9concrete2yyF'
public func concrete2() {
  generic([Int].self)
  generic([String].self)
}

class C {}

extension C: Equatable {
  static func ==(lhs: C, rhs: C) -> Bool {
    true
  }
}

extension C: Hashable {
  func hash(into hasher: inout Hasher) {}
}

struct Dumb {
  let x: Int
}

@_silgen_name("takeDumbKp")
func takeDumbKp(_: KeyPath<Dumb, Int>)

// CHECK-LABEL: sil @$s11cse_keypath5dumb1yyF : $@convention(thin) () -> () {
// CHECK:         [[KP:%.*]] = keypath $KeyPath<Dumb, Int>
// CHECK:         [[FN_REF:%.*]] = function_ref @takeDumbKp
// CHECK-NEXT:    [[APPLY_0:%.*]] = apply [[FN_REF]]([[KP]])
// CHECK-NEXT:    [[APPLY_1:%.*]] = apply [[FN_REF]]([[KP]])
// CHECK-NEXT:    strong_release [[KP]]
// CHECK-LABEL: } // end sil function '$s11cse_keypath5dumb1yyF'
public func dumb1() {
  takeDumbKp(\Dumb.x)
  takeDumbKp(\Dumb.x)
}
