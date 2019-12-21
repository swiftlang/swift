// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol Foo {
  static var button: Self { get }
}

enum Bar: Foo {
  case button
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21protocol_enum_witness3BarOAA3FooA2aDP6buttonxvgZTW : $@convention(witness_method: Foo) (@thick Bar.Type) -> @out Bar {
// CHECK-NEXT: bb0([[BAR:%.*]] : $*Bar, [[BAR_TYPE:%.*]] : $@thick Bar.Type):
// CHECK-NEXT: [[META_TYPE:%.*]] = metatype $@thin Bar.Type
// CHECK-NEXT: [[REF:%.*]] = function_ref @$s21protocol_enum_witness3BarO6buttonyA2CmF : $@convention(method) (@thin Bar.Type) -> Bar
// CHECK-NEXT: [[RESULT:%.*]] = apply [[REF]]([[META_TYPE]]) : $@convention(method) (@thin Bar.Type) -> Bar
// CHECK-NEXT store [[RESULT]] to [trivial] [[BAR]] : $*Bar
// CHECK-NEXT: [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT: return [[TUPLE]] : $()
// CHECK-END: }

// CHECK-LABEL: sil shared [transparent] [ossa] @$s21protocol_enum_witness3BarO6buttonyA2CmF : $@convention(method) (@thin Bar.Type) -> Bar {
// CHECK-NEXT: bb0({{%.*}} : $@thin Bar.Type):
// CHECK-NEXT: [[CASE:%.*]] = enum $Bar, #Bar.button!enumelt
// CHECK-NEXT: return [[CASE]] : $Bar
// CHECK-END: }

// CHECK-LABEL: sil_witness_table hidden Bar: Foo module protocol_enum_witness {
// CHECK: method #Foo.button!getter.1: <Self where Self : Foo> (Self.Type) -> () -> Self : @$s21protocol_enum_witness3BarOAA3FooA2aDP6buttonxvgZTW