// RUN: %target-swift-emit-silgen -module-name implicit_property_initializers -Xllvm -sil-full-demangle -enable-testing %s | %FileCheck %s

// CHECK: struct HasDefaultTupleOfNils {
// CHECK:   @_hasStorage @_hasInitialValue var x: (Int?, Int?)
// CHECK:   @_hasStorage @_hasInitialValue var y: Int?
// CHECK:   @_hasStorage var z: Int
// CHECK:   @_hasStorage @_hasInitialValue var w: ((Int?, (), Int?), (Int?, Int?))
// CHECK:   init(x: (Int?, Int?) = (nil, nil),
// CHECK-SAME:   y: Int? = nil,
// CHECK-SAME:   z: Int,
// CHECK-SAME:   w: ((Int?, (), Int?), (Int?, Int?)) = ((nil, (), nil), (nil, nil)))
// CHECK: }
struct HasDefaultTupleOfNils {
  var x: (Int?, Int?)
  var y: Int?
  var z: Int
  var w: ((Int?, (), Int?), (Int?, Int?))
}

// The default value initializer for 'x' should have type (Optional<Int>, Optional<Int>)

// CHECK: sil [transparent] [ossa] @$[[X_VALUE_INIT:s30implicit_property_initializers21HasDefaultTupleOfNilsV1xSiSg_AEtvpfi]] : $@convention(thin) () -> (Optional<Int>, Optional<Int>) {
// CHECK: bb0:
// CHECK:   %[[OPT1:[0-9]+]] = enum $Optional<Int>, #Optional.none!enumelt
// CHECK:   %[[OPT2:[0-9]+]] = enum $Optional<Int>, #Optional.none!enumelt
// CHECK:   %[[TUPLE:[0-9]+]] = tuple (%[[OPT1]] : $Optional<Int>, %[[OPT2]] : $Optional<Int>)
// CHECK:   return %[[TUPLE]] : $(Optional<Int>, Optional<Int>)
// CHECK: }

// The default value initializer for 'y' should have type Optional<Int>

//CHECK: sil [transparent] [ossa] @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1ySiSgvpfi : $@convention(thin) () -> Optional<Int> {
//CHECK: bb0:
//CHECK:   %[[OPT:[0-9]+]] = enum $Optional<Int>, #Optional.none!enumelt
//CHECK:   return %[[OPT]] : $Optional<Int>
//CHECK: }

// There should not be a default value initializer for 'z'.

// CHECK-NOT: @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1zSivpfi

// The default value initializer for 'w' should flatten to type (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)

//CHECK: sil [transparent] [ossa] @$[[W_VALUE_INIT:s30implicit_property_initializers21HasDefaultTupleOfNilsV1wSiSg_ytAEt_AE_AEttvpfi]] : $@convention(thin) () -> (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>) {
//CHECK: bb0:
//CHECK:   %[[OPT0:[0-9]+]] = enum $Optional<Int>, #Optional.none!enumelt
//CHECK:   %[[OPT1:[0-9]+]] = enum $Optional<Int>, #Optional.none!enumelt
//CHECK:   %[[OPT2:[0-9]+]] = enum $Optional<Int>, #Optional.none!enumelt
//CHECK:   %[[OPT3:[0-9]+]] = enum $Optional<Int>, #Optional.none!enumelt
//CHECK:   %[[TUPLE:[0-9]+]] = tuple (%[[OPT0]] : $Optional<Int>, %[[OPT1]] : $Optional<Int>, %[[OPT2]] : $Optional<Int>, %[[OPT3]] : $Optional<Int>)
//CHECK:   return %[[TUPLE]] : $(Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
//CHECK: }

// The default arg generator for 'x' inside the memberwise init should have type (Optional<Int>, Optional<Int>)

// CHECK: sil [ossa] @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1x1y1z1wACSiSg_AHt_AHSiAH_ytAHt_AH_AHtttcfcfA_ : $@convention(thin) () -> (Optional<Int>, Optional<Int>) {
// CHECK: bb0:
// CHECK:   %[[INIT_FN:[0-9]+]] = function_ref @$[[X_VALUE_INIT]] : $@convention(thin) () -> (Optional<Int>, Optional<Int>)
// CHECK:   %[[RESULT:[0-9]+]] = apply %[[INIT_FN]]() : $@convention(thin) () -> (Optional<Int>, Optional<Int>)
// CHECK:   (%[[OPT1:[0-9]+]], %[[OPT2:[0-9]+]]) = destructure_tuple %[[RESULT]] : $(Optional<Int>, Optional<Int>)
// CHECK:   %[[TUPLE:[0-9]]] = tuple (%[[OPT1]] : $Optional<Int>, %[[OPT2]] : $Optional<Int>)
// CHECK:   return %[[TUPLE]] : $(Optional<Int>, Optional<Int>)
// CHECK: }

// There should not be a default arg generator for 'y' because it's just a nil literal and clients construct it directly.

// CHECK-NOT: @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1x1y1z1wACSiSg_AHt_AHSiAH_ytAHt_AH_AHtttcfcfA0_

// There should not be a default arg generator for 'z'

// CHECK-NOT: @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1x1y1z1wACSiSg_AHt_AHSiAH_ytAHt_AH_AHtttcfcfA1_

// The default arg generator for 'w' should flatten to type (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)

// CHECK: sil [ossa] @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1x1y1z1wACSiSg_AHt_AHSiAH_ytAHt_AH_AHtttcfcfA2_ : $@convention(thin) () -> (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>) {
// CHECK: bb0:
// CHECK:   %[[INIT_FN:[0-9]+]] = function_ref @$[[W_VALUE_INIT]] : $@convention(thin) () -> (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
// CHECK:   %[[RESULT:[0-9]+]] = apply %[[INIT_FN:[0-9]+]]() : $@convention(thin) () -> (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
// CHECK:   (%[[OPT1:[0-9]+]], %[[OPT2:[0-9]+]], %[[OPT3:[0-9]+]], %[[OPT4:[0-9]+]]) = destructure_tuple %[[RESULT]] : $(Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
// CHECK:   %[[TUPLE:[0-9]]] = tuple (%[[OPT1]] : $Optional<Int>, %[[OPT2]] : $Optional<Int>, %[[OPT3]] : $Optional<Int>, %[[OPT4]] : $Optional<Int>)
// CHECK:   return %[[TUPLE]] : $(Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
// CHECK: }
