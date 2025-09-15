// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name cf -enable-objc-interop -import-cf-types -sdk %S/Inputs %s -o - | %FileCheck %s

import CoreCooling

// CHECK: sil hidden [ossa] @$s2cf8useEmAllyySo16CCMagnetismModelCF :
// CHECK: bb0([[ARG:%.*]] : @guaranteed $CCMagnetismModel):
func useEmAll(_ model: CCMagnetismModel) {
// CHECK: function_ref @CCPowerSupplyGetDefault : $@convention(c) () -> @autoreleased Optional<CCPowerSupply>
  let power = CCPowerSupplyGetDefault()

// CHECK: function_ref @CCRefrigeratorCreate : $@convention(c) (Optional<CCPowerSupply>) -> Optional<Unmanaged<CCRefrigerator>>
  let unmanagedFridge = CCRefrigeratorCreate(power)

// CHECK: function_ref @CCRefrigeratorSpawn : $@convention(c) (Optional<CCPowerSupply>) -> @owned Optional<CCRefrigerator>
  let managedFridge = CCRefrigeratorSpawn(power)

// CHECK: function_ref @CCRefrigeratorOpen : $@convention(c) (Optional<CCRefrigerator>) -> ()
  CCRefrigeratorOpen(managedFridge)

// CHECK: function_ref @CCRefrigeratorCopy : $@convention(c) (Optional<CCRefrigerator>) -> @owned Optional<CCRefrigerator>
  let copy = CCRefrigeratorCopy(managedFridge)

// CHECK: function_ref @CCRefrigeratorClone : $@convention(c) (Optional<CCRefrigerator>) -> @autoreleased Optional<CCRefrigerator>
  let clone = CCRefrigeratorClone(managedFridge)

// CHECK: function_ref @CCRefrigeratorDestroy : $@convention(c) (@owned Optional<CCRefrigerator>) -> ()
  CCRefrigeratorDestroy(clone)

// CHECK: objc_method [[ARG]] : $CCMagnetismModel, #CCMagnetismModel.refrigerator!foreign : (CCMagnetismModel) -> () -> Unmanaged<CCRefrigerator>?, $@convention(objc_method) (CCMagnetismModel) -> @unowned_inner_pointer Optional<Unmanaged<CCRefrigerator>>
  let f0 = model.refrigerator()

// CHECK: objc_method [[ARG]] : $CCMagnetismModel, #CCMagnetismModel.getRefrigerator!foreign : (CCMagnetismModel) -> () -> CCRefrigerator?, $@convention(objc_method) (CCMagnetismModel) -> @autoreleased Optional<CCRefrigerator>
  let f1 = model.getRefrigerator()

// CHECK: objc_method [[ARG]] : $CCMagnetismModel, #CCMagnetismModel.takeRefrigerator!foreign : (CCMagnetismModel) -> () -> CCRefrigerator?, $@convention(objc_method) (CCMagnetismModel) -> @owned Optional<CCRefrigerator>
  let f2 = model.takeRefrigerator()

// CHECK: objc_method [[ARG]] : $CCMagnetismModel, #CCMagnetismModel.borrowRefrigerator!foreign : (CCMagnetismModel) -> () -> CCRefrigerator?, $@convention(objc_method) (CCMagnetismModel) -> @autoreleased Optional<CCRefrigerator>
  let f3 = model.borrowRefrigerator()

// CHECK: objc_method [[ARG]] : $CCMagnetismModel, #CCMagnetismModel.setRefrigerator!foreign : (CCMagnetismModel) -> (CCRefrigerator?) -> (), $@convention(objc_method) (Optional<CCRefrigerator>, CCMagnetismModel) -> ()
  model.setRefrigerator(copy)

// CHECK: objc_method [[ARG]] : $CCMagnetismModel, #CCMagnetismModel.giveRefrigerator!foreign : (CCMagnetismModel) -> (CCRefrigerator?) -> (), $@convention(objc_method) (@owned Optional<CCRefrigerator>, CCMagnetismModel) -> ()
  model.giveRefrigerator(copy)

  // rdar://16846555
  let prop: CCRefrigerator = model.fridgeProp
}

// Ensure that accessors are emitted for fields used as protocol witnesses.
protocol Impedance {
  associatedtype Component
  var real: Component { get }
  var imag: Component { get }
}

extension CCImpedance: Impedance {}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$sSo11CCImpedanceV2cf9ImpedanceA2cDP4real9ComponentQzvgTW
// CHECK-LABEL: sil shared [transparent] [serialized] [ossa] @$sSo11CCImpedanceV4realSdvg
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$sSo11CCImpedanceV2cf9ImpedanceA2cDP4imag9ComponentQzvgTW
// CHECK-LABEL: sil shared [transparent] [serialized] [ossa] @$sSo11CCImpedanceV4imagSdvg

class MyMagnetism : CCMagnetismModel {
  // CHECK-LABEL: sil private [thunk] [ossa] @$s2cf11MyMagnetismC15getRefrigerator{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (MyMagnetism) -> @autoreleased CCRefrigerator
  override func getRefrigerator() -> CCRefrigerator {
    return super.getRefrigerator()
  }

  // CHECK-LABEL: sil private [thunk] [ossa] @$s2cf11MyMagnetismC16takeRefrigerator{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (MyMagnetism) -> @owned CCRefrigerator
  override func takeRefrigerator() -> CCRefrigerator {
    return super.takeRefrigerator()
  }

  // CHECK-LABEL: sil private [thunk] [ossa] @$s2cf11MyMagnetismC18borrowRefrigerator{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (MyMagnetism) -> @autoreleased CCRefrigerator
  override func borrowRefrigerator() -> CCRefrigerator {
    return super.borrowRefrigerator()
  }
}
