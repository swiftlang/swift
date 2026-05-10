// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK-%target-runtime --check-prefix=CHECK -DINT=i%target-ptrsize

protocol P0 {}
protocol P1 {}
struct P0Conformer: P0, Hashable {}
protocol CP0 : AnyObject {}
protocol CP1 : AnyObject {}
class C {}

struct S {
  var stored: Int
  var computed: Int {
    get { stored }
    set {}
  }
  subscript<T: P0>(t: T) -> Int {
    get { 0 }
    set {}
  }
}

enum SinglePayloadEnum<T> {
  case value(T)
  case different
  case otherwise
}

struct OutlinedOperations<T> {
  var first: T
  var second: T
  var third: T
  var fourth: T
}
struct StructHoldingOutlined<T> {
  var outlined: OutlinedOperations<T>
  var element: T
}

//   main
// CHECK-LABEL: define {{.*}} @{{main|__main_argc_argv}}(
// CHECK-SAME: [[ATTRS_SIMPLE:#[0-9]+]]

//   class deinit
// CHECK-LABEL: define {{.*}} @"$s30default_function_ir_attributes1CCfd"(
// CHECK-SAME: [[ATTRS_SIMPLE]]

//   outlined operation
// CHECK-LABEL: define {{.*}} @"$s30default_function_ir_attributes18OutlinedOperationsVyxGlWOc"(
// CHECK-SAME: [[ATTRS_NOINLINE_NOUNWIND:#[0-9]+]]

//   normal function
// CHECK-LABEL: define {{.*}} @"$s30default_function_ir_attributes3fooyyF"(
// CHECK-SAME: [[ATTRS_SIMPLE]]

func foo() {}

//   helper function: __swift_instantiateConcreteTypeFromMangledName
// CHECK-LABEL: define {{.*}} @__swift_instantiateConcreteTypeFromMangledName(
// CHECK-SAME: [[ATTRS_NOINLINE_READONLY_NOUNWIND_NOFRAME:#[0-9]+]]

func use_metadata() -> Any.Type {
  return ((C) -> Int).self
}

//   helper function: dynamic_cast_existential_1_unconditional
// CHECK-LABEL: define {{.*}} @dynamic_cast_existential_1_unconditional(
// CHECK-SAME: [[ATTRS_NOUNWIND:#[0-9]+]]

func test_class_existential_cast_0(value: AnyObject) -> CP0 {
  value as! CP0
}

//   helper function: dynamic_cast_existential_2_unconditional
// CHECK-LABEL: define {{.*}} @dynamic_cast_existential_2_unconditional(
// CHECK-SAME: [[ATTRS_NOUNWIND]]

func test_class_existential_cast_1(value: AnyObject) -> CP0 & CP1 {
  value as! CP0 & CP1
}

//   helper function: dynamic_cast_existential_2_conditional
// CHECK-LABEL: define {{.*}} @dynamic_cast_existential_2_conditional(
// CHECK-SAME: [[ATTRS_NOUNWIND]]

func test_class_existential_cast_2(value: AnyObject) -> (CP0 & CP1)? {
  value as? CP0 & CP1
}

//   helper function: dynamic_cast_existential_1_superclass_unconditional
// CHECK-LABEL: define {{.*}} @dynamic_cast_existential_1_superclass_unconditional(
// CHECK-SAME: [[ATTRS_NOUNWIND]]

func test_class_existential_cast_3(value: AnyObject) -> C & CP0 {
  value as! C & CP0
}

//   metadata accessor
// CHECK-LABEL: define {{.*}} @"$s30default_function_ir_attributes1CCMa"(
// CHECK-SAME: [[ATTRS_NOINLINE_READNONE_NOUNWIND_NOFRAME:#[0-9]+]]

//   helper function: dynamic_cast_existential_1_superclass_conditional
// CHECK-LABEL: define {{.*}} @dynamic_cast_existential_1_superclass_conditional(
// CHECK-SAME: [[ATTRS_NOUNWIND]]

func test_class_existential_cast_4(value: AnyObject) -> (C & CP0)? {
  value as? C & CP0
}

//   helper function: SIL-generated key path getter
// CHECK-LABEL: define {{.*}} @"$s30default_function_ir_attributes1SV8computedSivpACTK"(
// CHECK-SAME: [[ATTRS_SIMPLE]]

//   helper function: SIL-generated key path setter
// CHECK-LABEL: define {{.*}} @"$s30default_function_ir_attributes1SV8computedSivpACTk"(
// CHECK-SAME: [[ATTRS_SIMPLE]]

func test_computed_key_path_sil_thunks() -> KeyPath<S, Int> {
  \S.computed
}

//   helper function: IR-generated key path arg layout accessor
// CHECK-LABEL: define {{.*}} swiftcc {{.*}} @keypath_get_arg_layout(
// CHECK-SAME: [[ATTRS_SIMPLE]]

//   helper function: IR-generated key path destroy function
// CHECK-LABEL: define {{.*}} swiftcc {{.*}} @keypath_destroy(
// CHECK-SAME: [[ATTRS_SIMPLE]]

//   helper function: IR-generated key path copy function
// CHECK-LABEL: define {{.*}} swiftcc {{.*}} @keypath_copy(
// CHECK-SAME: [[ATTRS_SIMPLE]]

//   helper function: IR-generated key path argument initializer
// CHECK-LABEL: define {{.*}} swiftcc {{.*}} @keypath_arg_init(
// CHECK-SAME: [[ATTRS_SIMPLE]]

func test_computed_key_path_generic_thunks<T: P0 & Hashable>(value: T) -> KeyPath<S, Int> {
  return \S[value]
}

//   helper function: __swift_get_extra_inhabitant_index(
// CHECK-LABEL: define {{.*}} @__swift_get_extra_inhabitant_index(
// CHECK-SAME: [[ATTRS_SIMPLE]]

//   helper function: __swift_store_extra_inhabitant_index(
// CHECK-LABEL: define {{.*}} @__swift_store_extra_inhabitant_index(
// CHECK-SAME: [[ATTRS_SIMPLE]]

//   helper function: __swift_instantiateGenericMetadata
// CHECK-LABEL: define {{.*}} @__swift_instantiateGenericMetadata(
// CHECK-SAME: [[ATTRS_NOINLINE_READONLY_NOUNWIND_NOFRAME]]

//   Use the presence of a target-cpu attribute as a litmus for
//   whether constructInitialAttributes was called, since it's very
//   unlikely that handrolled code generation would think to add one.
// CHECK: attributes [[ATTRS_SIMPLE]] = { [[CUSTOM_ATTRS:.*target-cpu.*]] }{{$}}
// CHECK-DAG: attributes [[ATTRS_NOINLINE_NOUNWIND]] = { noinline nounwind {{.*target-cpu.*}} }
// CHECK-DAG: attributes [[ATTRS_NOINLINE_READNONE_NOUNWIND_NOFRAME]] = { noinline nounwind memory(none) {{.*}}"frame-pointer"="non-leaf"{{.*target-cpu.*}} }
// CHECK-DAG: attributes [[ATTRS_NOINLINE_READONLY_NOUNWIND_NOFRAME]] = { noinline nounwind willreturn memory(read) {{.*}}"frame-pointer"="non-leaf"{{.*target-cpu.*}} }
// CHECK-DAG: attributes [[ATTRS_NOUNWIND]] = { nounwind [[CUSTOM_ATTRS]] }{{$}}
