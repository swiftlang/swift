// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

protocol UID {
    func uid() -> Int
    var clsid: Int { get set }
    var iid: Int { get }
}

extension UID {
    var nextCLSID: Int {
      get { return clsid + 1 }
      set { clsid = newValue - 1 }
    }
}

protocol ObjectUID : class, UID {}

extension ObjectUID {
    var secondNextCLSID: Int {
      get { return clsid + 2 }
      set { }
    }
}


class Base {}

// CHECK-LABEL: sil hidden @_T025protocol_class_refinement12getObjectUID{{[_0-9a-zA-Z]*}}F
func getObjectUID<T: ObjectUID>(x: T) -> (Int, Int, Int, Int) {
  var x = x
  // CHECK: [[XBOX:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : ObjectUID> { var τ_0_0 } <T>
  // CHECK: [[PB:%.*]] = project_box [[XBOX]]
  // -- call x.uid()
  // CHECK: [[X:%.*]] = load [copy] [[PB]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [take] [[X_TMP]]
  // CHECK: destroy_value [[X2]]
  // -- call set x.clsid
  // CHECK: [[SET_CLSID:%.*]] = witness_method $T, #UID.clsid!setter.1
  // CHECK: apply [[SET_CLSID]]<T>([[UID]], [[PB]])
  x.clsid = x.uid()

  // -- call x.uid()
  // CHECK: [[X:%.*]] = load [copy] [[PB]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [take] [[X_TMP]]
  // CHECK: destroy_value [[X2]]
  // -- call nextCLSID from protocol ext
  // CHECK: [[SET_NEXTCLSID:%.*]] = function_ref @_T025protocol_class_refinement3UIDPAAE9nextCLSIDSifs
  // CHECK: apply [[SET_NEXTCLSID]]<T>([[UID]], [[PB]])
  x.nextCLSID = x.uid()

  // -- call x.uid()
  // CHECK: [[X1:%.*]] = load [copy] [[PB]]
  // CHECK: [[X:%.*]] = load [copy] [[PB]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [take] [[X_TMP]]
  // CHECK: destroy_value [[X2]]
  // -- call secondNextCLSID from class-constrained protocol ext
  // CHECK: [[SET_SECONDNEXT:%.*]] = function_ref @_T025protocol_class_refinement9ObjectUIDPAAE15secondNextCLSIDSifs
  // CHECK: [[BORROWED_X1:%.*]] = begin_borrow [[X1]]
  // CHECK: apply [[SET_SECONDNEXT]]<T>([[UID]], [[BORROWED_X1]])
  // CHECK: end_borrow [[BORROWED_X1]] from [[X1]]
  // CHECK: destroy_value [[X1]]
  x.secondNextCLSID = x.uid()
  return (x.iid, x.clsid, x.nextCLSID, x.secondNextCLSID)
}

// CHECK-LABEL: sil hidden @_T025protocol_class_refinement16getBaseObjectUID{{[_0-9a-zA-Z]*}}F
func getBaseObjectUID<T: UID where T: Base>(x: T) -> (Int, Int, Int) {
  var x = x
  // CHECK: [[XBOX:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : Base, τ_0_0 : UID> { var τ_0_0 } <T>
  // CHECK: [[PB:%.*]] = project_box [[XBOX]]
  // -- call x.uid()
  // CHECK: [[X:%.*]] = load [copy] [[PB]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [take] [[X_TMP]]
  // CHECK: destroy_value [[X2]]
  // -- call set x.clsid
  // CHECK: [[SET_CLSID:%.*]] = witness_method $T, #UID.clsid!setter.1
  // CHECK: apply [[SET_CLSID]]<T>([[UID]], [[PB]])
  x.clsid = x.uid()

  // -- call x.uid()
  // CHECK: [[X:%.*]] = load [copy] [[PB]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [take] [[X_TMP]]
  // CHECK: destroy_value [[X2]]
  // -- call nextCLSID from protocol ext
  // CHECK: [[SET_NEXTCLSID:%.*]] = function_ref @_T025protocol_class_refinement3UIDPAAE9nextCLSIDSifs
  // CHECK: apply [[SET_NEXTCLSID]]<T>([[UID]], [[PB]])
  x.nextCLSID = x.uid()
  return (x.iid, x.clsid, x.nextCLSID)
}
