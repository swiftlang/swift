// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol Foo {
  static var button: Self { get }
}

enum Bar: Foo {
  case button
}

protocol AnotherFoo {
  static func bar(arg: Int) -> Self
}

enum AnotherBar: AnotherFoo {
  case bar(arg: Int)
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21protocol_enum_witness3BarOAA3FooA2aDP6buttonxvgZTW : $@convention(witness_method: Foo) (@thick Bar.Type) -> @out Bar {
// CHECK: bb0([[BAR:%.*]] : $*Bar, [[BAR_TYPE:%.*]] : $@thick Bar.Type):
// CHECK-NEXT: [[META_TYPE:%.*]] = metatype $@thin Bar.Type
// CHECK: [[REF:%.*]] = function_ref @$s21protocol_enum_witness3BarO6buttonyA2CmF : $@convention(method) (@thin Bar.Type) -> Bar
// CHECK-NEXT: [[RESULT:%.*]] = apply [[REF]]([[META_TYPE]]) : $@convention(method) (@thin Bar.Type) -> Bar
// CHECK-NEXT: store [[RESULT]] to [trivial] [[BAR]] : $*Bar
// CHECK-NEXT: [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT: return [[TUPLE]] : $()
// CHECK-END: }

// CHECK-LABEL: sil shared [transparent] [ossa] @$s21protocol_enum_witness3BarO6buttonyA2CmF : $@convention(method) (@thin Bar.Type) -> Bar {
// CHECK: bb0({{%.*}} : $@thin Bar.Type):
// CHECK-NEXT: [[CASE:%.*]] = enum $Bar, #Bar.button!enumelt
// CHECK-NEXT: return [[CASE]] : $Bar
// CHECK-END: }

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s21protocol_enum_witness10AnotherBarOAA0D3FooA2aDP3bar3argxSi_tFZTW : $@convention(witness_method: AnotherFoo) (Int, @thick AnotherBar.Type) -> @out AnotherBar {
// CHECK: bb0([[ANOTHER_BAR:%.*]] : $*AnotherBar, [[INT_ARG:%.*]] : $Int, [[ANOTHER_BAR_TYPE:%.*]] : $@thick AnotherBar.Type):
// CHECK-NEXT: [[META_TYPE:%.*]] = metatype $@thin AnotherBar.Type
// CHECK: [[REF:%.*]] = function_ref @$s21protocol_enum_witness10AnotherBarO3baryACSi_tcACmF : $@convention(method) (Int, @thin AnotherBar.Type) -> AnotherBar
// CHECK-NEXT: [[RESULT:%.*]] = apply [[REF]]([[INT_ARG]], [[META_TYPE]]) : $@convention(method) (Int, @thin AnotherBar.Type) -> AnotherBar
// CHECK-NEXT: store [[RESULT]] to [trivial] [[ANOTHER_BAR]] : $*AnotherBar
// CHECK-NEXT: [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT: return [[TUPLE]] : $()
// CHECK-END: }

// CHECK-LABEL: sil_witness_table hidden Bar: Foo module protocol_enum_witness {
// CHECK: method #Foo.button!getter: <Self where Self : Foo> (Self.Type) -> () -> Self : @$s21protocol_enum_witness3BarOAA3FooA2aDP6buttonxvgZTW

// CHECK-LABEL: sil_witness_table hidden AnotherBar: AnotherFoo module protocol_enum_witness {
// CHECK: method #AnotherFoo.bar: <Self where Self : AnotherFoo> (Self.Type) -> (Int) -> Self : @$s21protocol_enum_witness10AnotherBarOAA0D3FooA2aDP3bar3argxSi_tFZTW
