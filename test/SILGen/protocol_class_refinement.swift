// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden [ossa] @$s25protocol_class_refinement12getObjectUID{{[_0-9a-zA-Z]*}}F
func getObjectUID<T: ObjectUID>(x: T) -> (Int, Int, Int, Int) {
  var x = x
  // CHECK: [[XBOX:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : ObjectUID> { var τ_0_0 } <T>
  // CHECK: [[XLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[XBOX]]
  // CHECK: [[PB:%.*]] = project_box [[XLIFETIME]]
  // -- call x.uid()
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK: [[X:%.*]] = load [copy] [[READ]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid :
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: destroy_addr [[X_TMP]]
  // -- call set x.clsid
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]] : $*T
  // CHECK: [[SET_CLSID:%.*]] = witness_method $T, #UID.clsid!setter
  // CHECK: apply [[SET_CLSID]]<T>([[UID]], [[WRITE]])
  x.clsid = x.uid()

  // -- call x.uid()
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK: [[X:%.*]] = load [copy] [[READ]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid :
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: destroy_addr [[X_TMP]]
  // -- call nextCLSID from protocol ext
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]] : $*T
  // CHECK: [[SET_NEXTCLSID:%.*]] = function_ref @$s25protocol_class_refinement3UIDPAAE9nextCLSIDSivs
  // CHECK: apply [[SET_NEXTCLSID]]<T>([[UID]], [[WRITE]])
  x.nextCLSID = x.uid()

  // -- call x.uid()
  // CHECK: [[READ1:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK: [[X1:%.*]] = load [copy] [[READ1]]
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK: [[X:%.*]] = load [copy] [[READ]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]

  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid :
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: destroy_addr [[X_TMP]]
  // -- call secondNextCLSID from class-constrained protocol ext
  // CHECK: [[SET_SECONDNEXT:%.*]] = function_ref @$s25protocol_class_refinement9ObjectUIDPAAE15secondNextCLSIDSivs
  // CHECK: apply [[SET_SECONDNEXT]]<T>([[UID]], [[X1]])
  // CHECK: destroy_value [[X1]]
  x.secondNextCLSID = x.uid()
  return (x.iid, x.clsid, x.nextCLSID, x.secondNextCLSID)

  // CHECK: return
}

// CHECK-LABEL: sil hidden [ossa] @$s25protocol_class_refinement16getBaseObjectUID{{[_0-9a-zA-Z]*}}F
func getBaseObjectUID<T: UID>(x: T) -> (Int, Int, Int) where T: Base {
  var x = x
  // CHECK: [[XBOX:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : Base, τ_0_0 : UID> { var τ_0_0 } <T>
  // CHECK: [[XLIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[XBOX]]
  // CHECK: [[PB:%.*]] = project_box [[XLIFETIME]]
  // -- call x.uid()
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK: [[X:%.*]] = load [copy] [[READ]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid :
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: destroy_addr [[X_TMP]]
  // -- call set x.clsid
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]] : $*T
  // CHECK: [[SET_CLSID:%.*]] = witness_method $T, #UID.clsid!setter
  // CHECK: apply [[SET_CLSID]]<T>([[UID]], [[WRITE]])
  x.clsid = x.uid()

  // -- call x.uid()
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PB]] : $*T
  // CHECK: [[X:%.*]] = load [copy] [[READ]]
  // CHECK: [[X_TMP:%.*]] = alloc_stack
  // CHECK: store [[X]] to [init] [[X_TMP]]
  // CHECK: [[GET_UID:%.*]] = witness_method $T, #UID.uid :
  // CHECK: [[UID:%.*]] = apply [[GET_UID]]<T>([[X_TMP]])
  // CHECK: destroy_addr [[X_TMP]]
  // -- call nextCLSID from protocol ext
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]] : $*T
  // CHECK: [[SET_NEXTCLSID:%.*]] = function_ref @$s25protocol_class_refinement3UIDPAAE9nextCLSIDSivs
  // CHECK: apply [[SET_NEXTCLSID]]<T>([[UID]], [[WRITE]])
  x.nextCLSID = x.uid()
  return (x.iid, x.clsid, x.nextCLSID)

  // CHECK: return
}
