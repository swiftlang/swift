// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-silgen %s -module-name cdecl \
// RUN:   -enable-experimental-feature CDecl > %t/out.sil
// RUN: %FileCheck %s -input-file %t/out.sil

// REQUIRES: swift_feature_CDecl

// CHECK-LABEL: sil hidden [thunk] [ossa] @pear : $@convention(c)
// CHECK:         function_ref @$s5cdecl5apple{{[_0-9a-zA-Z]*}}F
// CHECK-LABEL: sil hidden [ossa] @$s5cdecl5apple{{[_0-9a-zA-Z]*}}F
@cdecl(pear)
func apple(_ f: @convention(c) (Int) -> Int) { }

// CHECK-LABEL: sil hidden [ossa] @$s5cdecl16forceCEntryPoint{{[_0-9a-zA-Z]*}}F
// CHECK:         function_ref @grapefruit : $@convention(c) (Int) -> Int
// CHECK:         function_ref apple(_:)
// CHECK:         function_ref @$s5cdecl5appleyyS2iXCF : $@convention(thin) (@convention(c) (Int) -> Int) -> ()
// FIXME should it be function_ref @apple?
func forceCEntryPoint() {
  apple(orange)
}

// CHECK-LABEL: sil hidden [thunk] [ossa] @grapefruit : $@convention(c)
// CHECK:         function_ref @$s5cdecl6orange{{[_0-9a-zA-Z]*}}F
// CHECK-LABEL: sil hidden [ossa] @$s5cdecl6orange{{[_0-9a-zA-Z]*}}F
@cdecl(grapefruit)
func orange(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil [serialized] [thunk] [ossa] @cauliflower : $@convention(c)
// CHECK:         function_ref @$s5cdecl8broccoli{{[_0-9a-zA-Z]*}}F
// CHECK-LABEL: sil [ossa] @$s5cdecl8broccoli{{[_0-9a-zA-Z]*}}F
// FIXME should it be `sil hidden`?
@cdecl(cauliflower)
public func broccoli(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil private [thunk] [ossa] @collard_greens : $@convention(c)
// CHECK:         function_ref @$s5cdecl4kale[[PRIVATE:.*]]
// CHECK:       sil private [ossa] @$s5cdecl4kale[[PRIVATE:.*]]
@cdecl(collard_greens)
private func kale(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil private [thunk] [ossa] @defaultName : $@convention(c)
// CHECK:         function_ref @$s5cdecl11defaultName[[PRIVATE:.*]]
// CHECK:       sil private [ossa] @$s5cdecl11defaultName[[PRIVATE:.*]]
@cdecl
private func defaultName(_ x: Int) -> Int {
  return x
}
