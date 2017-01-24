// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -import-cf-types -sdk %S/Inputs %s -emit-silgen -o - | %FileCheck %s

// REQUIRES: objc_interop

import CoreCooling

// CHECK: sil hidden @_T02cf8useEmAllySo16CCMagnetismModelCF :
// CHECK: bb0([[ARG:%.*]] : $CCMagnetismModel):
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

// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $CCMagnetismModel, #CCMagnetismModel.refrigerator!1.foreign : (CCMagnetismModel) -> () -> Unmanaged<CCRefrigerator>!, $@convention(objc_method) (CCMagnetismModel) -> Optional<Unmanaged<CCRefrigerator>>
  let f0 = model.refrigerator()

// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $CCMagnetismModel, #CCMagnetismModel.getRefrigerator!1.foreign : (CCMagnetismModel) -> () -> CCRefrigerator!, $@convention(objc_method) (CCMagnetismModel) -> @autoreleased Optional<CCRefrigerator>
  let f1 = model.getRefrigerator()

// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $CCMagnetismModel, #CCMagnetismModel.takeRefrigerator!1.foreign : (CCMagnetismModel) -> () -> CCRefrigerator!, $@convention(objc_method) (CCMagnetismModel) -> @owned Optional<CCRefrigerator>
  let f2 = model.takeRefrigerator()

// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $CCMagnetismModel, #CCMagnetismModel.borrowRefrigerator!1.foreign : (CCMagnetismModel) -> () -> CCRefrigerator!, $@convention(objc_method) (CCMagnetismModel) -> @autoreleased Optional<CCRefrigerator>
  let f3 = model.borrowRefrigerator()

// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $CCMagnetismModel, #CCMagnetismModel.setRefrigerator!1.foreign : (CCMagnetismModel) -> (CCRefrigerator!) -> (), $@convention(objc_method) (Optional<CCRefrigerator>, CCMagnetismModel) -> ()
  model.setRefrigerator(copy)

// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $CCMagnetismModel, #CCMagnetismModel.giveRefrigerator!1.foreign : (CCMagnetismModel) -> (CCRefrigerator!) -> (), $@convention(objc_method) (@owned Optional<CCRefrigerator>, CCMagnetismModel) -> ()
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

// CHECK-LABEL: sil hidden [transparent] [fragile] [thunk] @_T0SC11CCImpedanceV2cf9ImpedanceAccDP4real9ComponentQzfgTW
// CHECK-LABEL: sil shared [transparent] [fragile] @_T0SC11CCImpedanceV4realSdfg
// CHECK-LABEL: sil hidden [transparent] [fragile] [thunk] @_T0SC11CCImpedanceV2cf9ImpedanceAccDP4imag9ComponentQzfgTW
// CHECK-LABEL: sil shared [transparent] [fragile] @_T0SC11CCImpedanceV4imagSdfg

class MyMagnetism : CCMagnetismModel {
  // CHECK-LABEL: sil hidden [thunk] @_T02cf11MyMagnetismC15getRefrigerator{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (MyMagnetism) -> @autoreleased CCRefrigerator
  override func getRefrigerator() -> CCRefrigerator {
    return super.getRefrigerator()
  }

  // CHECK-LABEL: sil hidden [thunk] @_T02cf11MyMagnetismC16takeRefrigerator{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (MyMagnetism) -> @owned CCRefrigerator
  override func takeRefrigerator() -> CCRefrigerator {
    return super.takeRefrigerator()
  }

  // CHECK-LABEL: sil hidden [thunk] @_T02cf11MyMagnetismC18borrowRefrigerator{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (MyMagnetism) -> @autoreleased CCRefrigerator
  override func borrowRefrigerator() -> CCRefrigerator {
    return super.borrowRefrigerator()
  }
}
