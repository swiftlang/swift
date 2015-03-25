// RUN: %target-swift-frontend -emit-silgen -parse-as-library %s | FileCheck %s

enum Boolish {
  case falsy
  case truthy
}

// CHECK-LABEL: sil hidden [transparent] @_TFO5union7Boolish5falsyFMS0_S0_ : $@thin (@thin Boolish.Type) -> Boolish {
// CHECK: bb0({{%.*}} : $@thin Boolish.Type):
// CHECK:   [[RES:%.*]] = enum $Boolish, #Boolish.falsy!enumelt
// CHECK:   return [[RES]] : $Boolish
// CHECK: }

// CHECK-LABEL: sil hidden [transparent] @_TFO5union7Boolish6truthyFMS0_S0_ : $@thin (@thin Boolish.Type) -> Boolish {
// CHECK: bb0({{%.*}} : $@thin Boolish.Type):
// CHECK:   [[RES:%.*]] = enum $Boolish, #Boolish.truthy!enumelt
// CHECK:   return [[RES]] : $Boolish
// CHECK: }

enum Optionable {
  case nought
  case mere(Int)
}

// CHECK-LABEL: sil hidden [transparent] @_TFO5union10Optionable6noughtFMS0_S0_ : $@thin (@thin Optionable.Type) -> Optionable {
// CHECK: bb0({{%.*}} : $@thin Optionable.Type):
// CHECK:   [[RES:%.*]] = enum $Optionable, #Optionable.nought!enumelt
// CHECK:   return [[RES]] : $Optionable
// CHECK: }

// CHECK-LABEL: sil hidden [transparent] @_TFO5union10Optionable4merefMS0_FSiS0_ : $@thin (Int, @thin Optionable.Type) -> Optionable {
// CHECK: bb0([[ARG:%.*]] : $Int, {{%.*}} : $@thin Optionable.Type):
// CHECK:   [[RES:%.*]] = enum $Optionable, #Optionable.mere!enumelt.1, [[ARG]] : $Int
// CHECK:   return [[RES]] : $Optionable
// CHECK: }

// CHECK-LABEL: sil hidden  @_TF5uniong6truthyOS_7Boolish
var truthy : Boolish {
  // CHECK: [[TRUTHY:%[0-9]+]] = function_ref @_TFO5union7Boolish6truthyFMS0_S0_
  // CHECK: [[BOOLISH:%[0-9]+]] = metatype $@thin Boolish.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[TRUTHY]]([[BOOLISH]])
  // CHECK: return [[RESULT]]
  return .truthy
}

// CHECK-LABEL: sil hidden  @_TF5uniong5falsyOS_7Boolish
var falsy : Boolish {
  // CHECK: [[FALSY:%[0-9]+]] = function_ref @_TFO5union7Boolish5falsyFMS0_S0_
  // CHECK: [[BOOLISH:%[0-9]+]] = metatype $@thin Boolish.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[FALSY]]([[BOOLISH]])
  // CHECK: return [[RESULT]]
  return .falsy
}

protocol P {}

enum AddressOnly {
  case nought
  case mere(P)
}

// CHECK-LABEL: sil hidden [transparent] @_TFO5union11AddressOnly6noughtFMS0_S0_ : $@thin (@out AddressOnly, @thin AddressOnly.Type) -> () {
// CHECK: bb0([[RET:%.*]] : $*AddressOnly, {{%.*}} : $@thin AddressOnly.Type):
// CHECK:   inject_enum_addr [[RET]] : $*AddressOnly, #AddressOnly.nought!enumelt
// CHECK:   return
// CHECK: }

// CHECK-LABEL: sil hidden [transparent] @_TFO5union11AddressOnly4merefMS0_FPS_1P_S0_ : $@thin (@out AddressOnly, @in P, @thin AddressOnly.Type) -> () {
// CHECK: bb0([[RET:%.*]] : $*AddressOnly, [[DATA:%.*]] : $*P, {{%.*}} : $@thin AddressOnly.Type):
// CHECK:   [[RET_DATA:%.*]] = init_enum_data_addr [[RET]] : $*AddressOnly, #AddressOnly.mere!enumelt.1 // user: %4
// CHECK:   copy_addr [take] [[DATA]] to [initialization] [[RET_DATA]] : $*P
// CHECK:   inject_enum_addr [[RET]] : $*AddressOnly, #AddressOnly.mere!enumelt.1
// CHECK:   return
// CHECK: }
