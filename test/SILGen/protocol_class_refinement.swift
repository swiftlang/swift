// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol UID {
    func uid() -> Int
    var clsid: Int { get set }
    var iid: Int { get }
}

protocol ObjectUID : class, UID {}

class Base {}

// CHECK-LABEL: sil hidden @_TF25protocol_class_refinement12getObjectUIDUS_9ObjectUID__FQ_TSiSi_
// CHECK:       bb0([[X:%.*]] : $T):
// CHECK-NOT:         strong_retain [[X]]
// -- call x.uid()
// CHECK:         [[TMP:%.*]] = alloc_stack $T
// CHECK:         store [[X]] to [[TMP]]
// CHECK:         [[UID:%.*]] = witness_method $T, #UID.uid
// CHECK:         [[UID_VALUE:%.*]] = apply [[UID]]<T>([[TMP]]#1)
// CHECK-NOT:     strong_release [[X]]
// -- call x.clsid.setter (TODO: avoid r/r here)
// CHECK:         [[TMP:%.*]] = alloc_stack $T
// CHECK:         store [[X]] to [[TMP]]
// CHECK:         [[SET_CLSID:%.*]] = witness_method $T, #UID.clsid!setter
// CHECK:         apply [[SET_CLSID]]<T>([[UID_VALUE]], [[TMP]]#1)
// CHECK:         dealloc_stack [[TMP]]#0
// -- call x.iid.getter
// CHECK-NOT:     strong_retain [[X]]
// CHECK:         [[TMP:%.*]] = alloc_stack $T
// CHECK:         store [[X]] to [[TMP]]
// CHECK:         [[GET_IID:%.*]] = witness_method $T, #UID.iid!getter
// CHECK:         apply [[GET_IID]]<T>([[TMP]]#1)
// CHECK-NOT:     strong_release [[X]]
// -- call x.clsid.getter (TODO: avoid r/r here)
// CHECK-NOT:         strong_retain [[X]]
// CHECK:         [[TMP:%.*]] = alloc_stack $T
// CHECK:         store [[X]] to [[TMP]]
// CHECK:         [[GET_CLSID:%.*]] = witness_method $T, #UID.clsid!getter
// CHECK:         apply [[GET_CLSID]]<T>([[TMP]]#1)
// CHECK:         dealloc_stack [[TMP]]#0
// -- done
// CHECK:         strong_release [[X]]

func getObjectUID<T: ObjectUID>(x: T) -> (Int, Int) {
  x.clsid = x.uid()
  return (x.iid, x.clsid)
}

// CHECK-LABEL: sil hidden @_TF25protocol_class_refinement16getBaseObjectUIDUS_3UID__FQ_TSiSi_
// CHECK:       bb0([[X:%.*]] : $T):
// CHECK-NOT:         strong_retain [[X]]
// -- call x.uid()
// CHECK:         [[TMP:%.*]] = alloc_stack $T
// CHECK:         store [[X]] to [[TMP]]
// CHECK:         [[UID:%.*]] = witness_method $T, #UID.uid
// CHECK:         [[UID_VALUE:%.*]] = apply [[UID]]<T>([[TMP]]#1)
// CHECK-NOT:     strong_release [[X]]
// -- call x.clsid.setter (TODO: avoid r/r here)
// CHECK:         [[TMP:%.*]] = alloc_stack $T
// CHECK:         store [[X]] to [[TMP]]
// CHECK:         [[SET_CLSID:%.*]] = witness_method $T, #UID.clsid!setter
// CHECK:         apply [[SET_CLSID]]<T>([[UID_VALUE]], [[TMP]]#1)
// CHECK-NOT:     strong_release [[X]]
// CHECK:         dealloc_stack [[TMP]]#0
// CHECK-NOT:     strong_release [[X]]
// -- call x.iid.getter
// CHECK-NOT:     strong_retain [[X]]
// CHECK:         [[TMP0:%.*]] = alloc_stack $T
// CHECK:         store [[X]] to [[TMP0]]
// CHECK:         [[GET_IID:%.*]] = witness_method $T, #UID.iid!getter
// CHECK:         apply [[GET_IID]]<T>([[TMP0]]#1)
// CHECK-NOT:     strong_release [[X]]
// CHECK-NOT:     destroy_addr [[TMP0]]
// -- call x.clsid.getter (TODO: avoid r/r here)
// CHECK:         [[TMP1:%.*]] = alloc_stack $T
// CHECK:         store [[X]] to [[TMP1]]
// CHECK:         [[GET_CLSID:%.*]] = witness_method $T, #UID.clsid!getter
// CHECK:         apply [[GET_CLSID]]<T>([[TMP1]]#1)
// CHECK-NOT:     strong_release [[X]]
// CHECK-NOT:     destroy_addr [[TMP0]]
// CHECK-NOT:     strong_release [[X]]
// CHECK-NOT:     destroy_addr [[TMP0]]
// CHECK:         dealloc_stack [[TMP1]]#0
// CHECK:         dealloc_stack [[TMP0]]#0
// -- done
// CHECK:         strong_release [[X]]

func getBaseObjectUID<T: UID where T: Base>(x: T) -> (Int, Int) {
  x.clsid = x.uid()
  return (x.iid, x.clsid)
}
