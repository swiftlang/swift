// RUN: %target-swift-emit-silgen -primary-file %s %S/Inputs/lazy_properties_other.swift -module-name lazy_properties_multi | %FileCheck %s
// RUN: %target-swift-emit-silgen %S/Inputs/lazy_properties_other.swift -primary-file %s -module-name lazy_properties_multi | %FileCheck %s
// RUN: %target-swift-emit-silgen %S/Inputs/lazy_properties_other.swift %s -module-name lazy_properties_multi | %FileCheck %s
// RUN: %target-swift-emit-silgen %s %S/Inputs/lazy_properties_other.swift -module-name lazy_properties_multi | %FileCheck %s


public class C1 {
  // CHECK-LABEL: sil [ossa] @$s21lazy_properties_multi2C1C1f1cyAA2C2C_tF : $@convention(method) (@guaranteed C2, @guaranteed C1) -> () {
  // CHECK: [[FN:%.*]] = function_ref @$s21lazy_properties_multi2C2C1fyyF : $@convention(method) (@guaranteed C2)
  // CHECK: apply [[FN]](%0) : $@convention(method) (@guaranteed C2) -> ()
  public func f(c: C2) {
    c.f()
  }
}
