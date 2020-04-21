// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class Foo {
  lazy var bar: Int = 0 {
    didSet { print(oldValue) }
    willSet { print(newValue) }
  }

  lazy var baz: Int = 0 {
    didSet { }
    willSet { print(newValue) }
  }
}

// Foo.bar.setter

// CHECK-LABEL: sil hidden [ossa] @$s28lazy_property_with_observers3FooC3barSivs : $@convention(method) (Int, @guaranteed Foo) -> () {
// CHECK: bb0([[VALUE:%.*]] : $Int, [[FOO:%.*]] : @guaranteed $Foo):
// CHECK-NEXT:  debug_value [[VALUE]] : $Int, let, name "value", argno 1 // id: %2
// CHECK-NEXT:  debug_value [[FOO]] : $Foo, let, name "self", argno 2 // id: %3
// CHECK-NEXT:  [[GETTER:%.*]] = class_method [[FOO]] : $Foo, #Foo.bar!getter : (Foo) -> () -> Int, $@convention(method) (@guaranteed Foo) -> Int
// CHECK-NEXT:  [[OLDVALUE:%.*]] = apply [[GETTER]]([[FOO]]) : $@convention(method) (@guaranteed Foo) -> Int
// CHECK-NEXT:  debug_value [[OLDVALUE]] : $Int, let, name "tmp"
// CHECK-NEXT:  // function_ref Foo.bar.willset
// CHECK-NEXT:  [[WILLSET:%.*]] = function_ref @$s28lazy_property_with_observers3FooC3barSivw : $@convention(method) (Int, @guaranteed Foo) -> ()
// CHECK-NEXT:  [[WILLSET_RESULT:%.*]] = apply [[WILLSET]]([[VALUE]], [[FOO]]) : $@convention(method) (Int, @guaranteed Foo) -> ()
// CHECK-NEXT:  [[ENUM:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, [[VALUE]] : $Int
// CHECK-NEXT:  [[REF_ELEM:%.*]] = ref_element_addr [[FOO]] : $Foo, #Foo.$__lazy_storage_$_bar
// CHECK-NEXT:  [[BEGIN_ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF_ELEM]] : $*Optional<Int>
// CHECK-NEXT:  assign [[ENUM]] to [[BEGIN_ACCESS]] : $*Optional<Int>
// CHECK-NEXT:  end_access [[BEGIN_ACCESS]] : $*Optional<Int>
// CHECK-NEXT:  // function_ref Foo.bar.didset
// CHECK-NEXT:  [[DIDSET:%.*]] = function_ref @$s28lazy_property_with_observers3FooC3barSivW : $@convention(method) (Int, @guaranteed Foo) -> ()
// CHECK-NEXT:  [[DIDSET_RESULT:%.*]] = apply [[DIDSET]]([[OLDVALUE]], [[FOO]]) : $@convention(method) (Int, @guaranteed Foo) -> ()
// CHECK-NEXT:  [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT:  return [[TUPLE]] : $()
// CHECK-END: }

// Foo.baz.setter

// CHECK-LABEL: sil hidden [ossa] @$s28lazy_property_with_observers3FooC3bazSivs : $@convention(method) (Int, @guaranteed Foo) -> () {
// CHECK: bb0([[VALUE:%.*]] : $Int, [[FOO:%.*]] : @guaranteed $Foo):
// CHECK-NEXT:  debug_value [[VALUE]] : $Int, let, name "value", argno 1
// CHECK-NEXT:  debug_value [[FOO]] : $Foo, let, name "self", argno 2
// CHECK-NEXT:  // function_ref Foo.baz.willset
// CHECK-NEXT:  [[WILLSET:%.*]] = function_ref @$s28lazy_property_with_observers3FooC3bazSivw : $@convention(method) (Int, @guaranteed Foo) -> ()
// CHECK-NEXT:  [[WILLSET_RESULT:%.*]] = apply [[WILLSET]]([[VALUE]], [[FOO]]) : $@convention(method) (Int, @guaranteed Foo) -> ()
// CHECK-NEXT:  [[ENUM:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, [[VALUE]] : $Int
// CHECK-NEXT:  [[REF_ELEM:%.*]] = ref_element_addr [[FOO]] : $Foo, #Foo.$__lazy_storage_$_baz
// CHECK-NEXT:  [[BEGIN_ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF_ELEM]] : $*Optional<Int>
// CHECK-NEXT:  assign [[ENUM]] to [[BEGIN_ACCESS]] : $*Optional<Int>
// CHECK-NEXT:  end_access [[BEGIN_ACCESS]] : $*Optional<Int>
// CHECK-NEXT:  // function_ref Foo.baz.didset
// CHECK-NEXT:  [[DIDSET:%.*]] = function_ref @$s28lazy_property_with_observers3FooC3bazSivW : $@convention(method) (@guaranteed Foo) -> ()
// CHECK-NEXT:  [[DIDSET_RESULT:%.*]] = apply [[DIDSET]]([[FOO]]) : $@convention(method) (@guaranteed Foo) -> ()
// CHECK-NEXT:  [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT:  return [[TUPLE]] : $()
// CHECK-END: }
