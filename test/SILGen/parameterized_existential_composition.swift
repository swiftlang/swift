// RUN: %target-swift-emit-silgen -disable-availability-checking %s | %FileCheck %s

protocol P<A> {
  associatedtype A
}

protocol Q<B> {
  associatedtype B
}

// All of these should have unique mangling.
func overload(_: any P<Int> & Q<String>) {}
func overload(_: any P<Float> & Q<String>) {}
func overload(_: any P & Q<String>) {}
func overload(_: any P<Int> & Q<Bool>) {}
func overload(_: any P<Float> & Q<Bool>) {}
func overload(_: any P & Q<Bool>) {}
func overload(_: any P<Int> & Q) {}
func overload(_: any P<Float> & Q) {}
func overload(_: any P & Q) {}
func overload(_: any P<Int>) {}
func overload(_: any P<Float>) {}
func overload(_: any P) {}
func overload(_: any Q<Bool>) {}
func overload(_: any Q) {}

// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpSi1AAaCPRts_SS1BAaDPRtsXPF : $@convention(thin) (@in_guaranteed any P<Int> & Q<String>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpSf1AAaCPRts_SS1BAaDPRtsXPF : $@convention(thin) (@in_guaranteed any P<Float> & Q<String>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpSS1BAaDPRts_XPF : $@convention(thin) (@in_guaranteed any P & Q<String>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpSi1AAaCPRts_Sb1BAaDPRtsXPF : $@convention(thin) (@in_guaranteed any P<Int> & Q<Bool>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpSf1AAaCPRts_Sb1BAaDPRtsXPF : $@convention(thin) (@in_guaranteed any P<Float> & Q<Bool>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpSb1BAaDPRts_XPF : $@convention(thin) (@in_guaranteed any P & Q<Bool>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpSi1AAaCPRts_XPF : $@convention(thin) (@in_guaranteed any P<Int> & Q) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpSf1AAaCPRts_XPF : $@convention(thin) (@in_guaranteed any P<Float> & Q) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_AA1QpF : $@convention(thin) (@in_guaranteed any P & Q) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_pSi1AAaCPRts_XPF : $@convention(thin) (@in_guaranteed any P<Int>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_pSf1AAaCPRts_XPF : $@convention(thin) (@in_guaranteed any P<Float>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1P_pF : $@convention(thin) (@in_guaranteed any P) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1Q_pSb1BAaCPRts_XPF : $@convention(thin) (@in_guaranteed any Q<Bool>) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s37parameterized_existential_composition8overloadyyAA1Q_pF : $@convention(thin) (@in_guaranteed any Q) -> () {
