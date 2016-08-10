// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @_TF25protocol_class_refinement12getObjectUID
func getObjectUID<T: ObjectUID>(x: T) -> (Int, Int, Int, Int) {
  var x = x
  // CHECK: [[XBOX:%.*]] = alloc_box $T
  // CHECK: [[PB:%.*]] = project_box [[XBOX]]
  // -- call x.uid()
  // CHECK: [[X:%.*]] = load [[PB]]
  // CHECK: strong_retain [[X]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [[X_TMP]]
  // CHECK: strong_release [[X2]]
  // -- call set x.clsid
  // CHECK: [[SET_CLSID:%.*]] = witness_method $T, #UID.clsid!setter.1
  // CHECK: apply [[SET_CLSID]]<T>([[UID]], [[PB]])
  x.clsid = x.uid()

  // -- call x.uid()
  // CHECK: [[X:%.*]] = load [[PB]]
  // CHECK: strong_retain [[X]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [[X_TMP]]
  // CHECK: strong_release [[X2]]
  // -- call nextCLSID from protocol ext
  // CHECK: [[SET_NEXTCLSID:%.*]] = function_ref @_TFE25protocol_class_refinementPS_3UIDs9nextCLSIDSi
  // CHECK: apply [[SET_NEXTCLSID]]<T>([[UID]], [[PB]])
  x.nextCLSID = x.uid()

  // -- call x.uid()
  // CHECK: [[X1:%.*]] = load [[PB]]
  // CHECK: strong_retain [[X1]]
  // CHECK: [[X:%.*]] = load [[PB]]
  // CHECK: strong_retain [[X]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [[X_TMP]]
  // CHECK: strong_release [[X2]]
  // -- call secondNextCLSID from class-constrained protocol ext
  // CHECK: [[SET_SECONDNEXT:%.*]] = function_ref @_TFE25protocol_class_refinementPS_9ObjectUIDs15secondNextCLSIDSi
  // CHECK: apply [[SET_SECONDNEXT]]<T>([[UID]], [[X1]])
  // CHECK: strong_release [[X1]]
  x.secondNextCLSID = x.uid()
  return (x.iid, x.clsid, x.nextCLSID, x.secondNextCLSID)
}

// CHECK-LABEL: sil hidden @_TF25protocol_class_refinement16getBaseObjectUID
func getBaseObjectUID<T: UID where T: Base>(x: T) -> (Int, Int, Int) {
  var x = x
  // CHECK: [[XBOX:%.*]] = alloc_box $T
  // CHECK: [[PB:%.*]] = project_box [[XBOX]]
  // -- call x.uid()
  // CHECK: [[X:%.*]] = load [[PB]]
  // CHECK: strong_retain [[X]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [[X_TMP]]
  // CHECK: strong_release [[X2]]
  // -- call set x.clsid
  // CHECK: [[SET_CLSID:%.*]] = witness_method $T, #UID.clsid!setter.1
  // CHECK: apply [[SET_CLSID]]<T>([[UID]], [[PB]])
  x.clsid = x.uid()

  // -- call x.uid()
  // CHECK: [[X:%.*]] = load [[PB]]
  // CHECK: strong_retain [[X]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid!1
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: [[X2:%.*]] = load [[X_TMP]]
  // CHECK: strong_release [[X2]]
  // -- call nextCLSID from protocol ext
  // CHECK: [[SET_NEXTCLSID:%.*]] = function_ref @_TFE25protocol_class_refinementPS_3UIDs9nextCLSIDSi
  // CHECK: apply [[SET_NEXTCLSID]]<T>([[UID]], [[PB]])
  x.nextCLSID = x.uid()
  return (x.iid, x.clsid, x.nextCLSID)
}
