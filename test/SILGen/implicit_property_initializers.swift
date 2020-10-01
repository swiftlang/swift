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
// CHECK:   %0 = enum $Optional<Int>, #Optional.none!enumelt
// CHECK:   %1 = enum $Optional<Int>, #Optional.none!enumelt
// CHECK:   %2 = tuple (%0 : $Optional<Int>, %1 : $Optional<Int>)
// CHECK:   return %2 : $(Optional<Int>, Optional<Int>)
// CHECK: }

// The default value initializer for 'y' should have type Optional<Int>

//CHECK: sil [transparent] [ossa] @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1ySiSgvpfi : $@convention(thin) () -> Optional<Int> {
//CHECK: bb0:
//CHECK:   %0 = enum $Optional<Int>, #Optional.none!enumelt // user: %1
//CHECK:   return %0 : $Optional<Int>                      // id: %1
//CHECK: }

// There should not be a default value initializer for 'z'.

// CHECK-NOT: @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1zSivpfi

// The default value initializer for 'w' should flatten to type (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)

//CHECK: sil [transparent] [ossa] @$[[W_VALUE_INIT:s30implicit_property_initializers21HasDefaultTupleOfNilsV1wSiSg_ytAEt_AE_AEttvpfi]] : $@convention(thin) () -> (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>) {
//CHECK: bb0:
//CHECK:   %0 = enum $Optional<Int>, #Optional.none!enumelt // user: %4
//CHECK:   %1 = enum $Optional<Int>, #Optional.none!enumelt // user: %4
//CHECK:   %2 = enum $Optional<Int>, #Optional.none!enumelt // user: %4
//CHECK:   %3 = enum $Optional<Int>, #Optional.none!enumelt // user: %4
//CHECK:   %4 = tuple (%0 : $Optional<Int>, %1 : $Optional<Int>, %2 : $Optional<Int>, %3 : $Optional<Int>) // user: %5
//CHECK:   return %4 : $(Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>) // id: %5
//CHECK: }

// The default arg generator for 'x' inside the memberwise init should have type (Optional<Int>, Optional<Int>)

// CHECK: sil [ossa] @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1x1y1z1wACSiSg_AHt_AHSiAH_ytAHt_AH_AHtttcfcfA_ : $@convention(thin) () -> (Optional<Int>, Optional<Int>) {
// CHECK: bb0:
// CHECK:   %0 = function_ref @$[[X_VALUE_INIT]] : $@convention(thin) () -> (Optional<Int>, Optional<Int>) // user: %1
// CHECK:   %1 = apply %0() : $@convention(thin) () -> (Optional<Int>, Optional<Int>)
// CHECK:   (%2, %3) = destructure_tuple %1 : $(Optional<Int>, Optional<Int>)
// CHECK:   %4 = tuple (%2 : $Optional<Int>, %3 : $Optional<Int>)
// CHECK:   return %4 : $(Optional<Int>, Optional<Int>)
// CHECK: }

// There should not be a default arg generator for 'y' because it's just a nil literal and clients construct it directly.

// CHECK-NOT: @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1x1y1z1wACSiSg_AHt_AHSiAH_ytAHt_AH_AHtttcfcfA0_

// There should not be a default arg generator for 'z'

// CHECK-NOT: @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1x1y1z1wACSiSg_AHt_AHSiAH_ytAHt_AH_AHtttcfcfA1_

// The default arg generator for 'w' should flatten to type (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)

// CHECK: sil [ossa] @$s30implicit_property_initializers21HasDefaultTupleOfNilsV1x1y1z1wACSiSg_AHt_AHSiAH_ytAHt_AH_AHtttcfcfA2_ : $@convention(thin) () -> (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>) {
// CHECK: bb0:
// CHECK:   %0 = function_ref @$[[W_VALUE_INIT]] : $@convention(thin) () -> (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
// CHECK:   %1 = apply %0() : $@convention(thin) () -> (Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
// CHECK:   (%2, %3, %4, %5) = destructure_tuple %1 : $(Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
// CHECK:   %6 = tuple (%2 : $Optional<Int>, %3 : $Optional<Int>, %4 : $Optional<Int>, %5 : $Optional<Int>)
// CHECK:   return %6 : $(Optional<Int>, Optional<Int>, Optional<Int>, Optional<Int>)
// CHECK: }
