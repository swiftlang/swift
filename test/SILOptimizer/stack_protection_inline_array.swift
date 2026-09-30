// RUN: %target-swift-frontend -module-name=test -emit-sil %s -O -disable-availability-checking -parse-as-library | %FileCheck %s --check-prefix=CHECKED
// RUN: %target-swift-frontend -module-name=test -emit-sil %s -Ounchecked -disable-availability-checking -parse-as-library | %FileCheck %s --check-prefix=OUNCHECKED

// Regression test: an InlineArray access that's provably in-bounds (checked
// subscript, or an initializer index drawn from `0 ..< count`) must not get
// `[stack_protection]` while one that isn't provably in-bounds (`unchecked`
// subscript) must keep it.

// safe under -O; regains protection under -Ounchecked, when `_checkIndex`
// compiles away
// CHECKED-LABEL: sil @$s4test17safeLocalMutation1vs11InlineArrayVy$1_SiGSi_tF : $@convention(thin) (Int) -> InlineArray<2, Int> {
// CHECKED-NOT: [stack_protection]
// CHECKED: } // end sil function
// OUNCHECKED-LABEL: sil [stack_protection] @$s4test17safeLocalMutation1vs11InlineArrayVy$1_SiGSi_tF : $@convention(thin) (Int) -> InlineArray<2, Int> {
public func safeLocalMutation(v: Int) -> InlineArray<2, Int> {
  var ints: InlineArray<2, Int> = [0, 0]
  ints[0] = v
  ints[1] = v
  return ints
}

// safe in both configurations: bounds-checked by construction (`0 ..< count`)
// CHECKED-LABEL: sil @$s4test23safeLocalInitialization1vs11InlineArrayVy$1_SiGSi_tF : $@convention(thin) (Int) -> InlineArray<2, Int> {
// CHECKED-NOT: [stack_protection]
// CHECKED: } // end sil function
// OUNCHECKED-LABEL: sil @$s4test23safeLocalInitialization1vs11InlineArrayVy$1_SiGSi_tF : $@convention(thin) (Int) -> InlineArray<2, Int> {
// OUNCHECKED-NOT: [stack_protection]
// OUNCHECKED: } // end sil function
public func safeLocalInitialization(v: Int) -> InlineArray<2, Int> {
  InlineArray<2, Int> { _ in v }
}

// unsafe in both configurations: index never proven in bounds
// CHECKED-LABEL: sil [stack_protection] @$s4test22uncheckedLocalMutation1v5indexs11InlineArrayVy$1_SiGSi_SitF : $@convention(thin) (Int, Int) -> InlineArray<2, Int> {
// CHECKED: index_addr [stack_protection] [projection] {{%[0-9]+}}, {{%[0-9]+}}
// CHECKED: } // end sil function
// OUNCHECKED-LABEL: sil [stack_protection] @$s4test22uncheckedLocalMutation1v5indexs11InlineArrayVy$1_SiGSi_SitF : $@convention(thin) (Int, Int) -> InlineArray<2, Int> {
// OUNCHECKED: index_addr [stack_protection] [projection] {{%[0-9]+}}, {{%[0-9]+}}
// OUNCHECKED: } // end sil function
public func uncheckedLocalMutation(v: Int, index: Int) -> InlineArray<2, Int> {
  var ints: InlineArray<2, Int> = [0, 0]
  ints[unchecked: index] = v
  return ints
}
