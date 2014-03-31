// RUN: %swift %s -O3 -sil-debug-serialization -o - -emit-sil | FileCheck %s

// CHECK-DAG: sil public_external @_TFSaCU__fMGSaQ__FT_GSaQ__ : $@thin <T> (@thin Array<T>.Type) -> @owned Array<T> {
// CHECK-DAG: sil public_external @_TFSa6appendU__fRGSaQ__FT5valueQ__T_ : $@cc(method) @thin <T> (@in T, @inout Array<T>) -> () {
// CHECK-DAG: sil public_external [transparent] @_TFVSs5UInt833_convertFromBuiltinIntegerLiteralfMS_FT5valueBi2048__S_ : $@thin (Builtin.Int2048, @thin UInt8.Type) -> UInt8 {
// CHECK-DAG: sil public_external @_TFVSs13UnsafePointer4nullU__fMGS_Q__FT_GS_Q__ : $@thin <T> (@thin UnsafePointer<T>.Type) -> UnsafePointer<T> {
// CHECK-DAG: sil public_external @_TFSs11arrayBufferU__FTMQ_5countSi8capacitySi_GVSs10HeapBufferVSs16CountAndCapacityQ__ : $@thin <T> (@thick T.Type, Int, Int) -> @owned HeapBuffer<CountAndCapacity, T> {
// CHECK-DAG: sil public_external @_TFVSs10HeapBufferg14elementStorageGVSs13UnsafePointerQ0__ : $@cc(method) @thin <Value, Element> (@owned HeapBuffer<Value, Element>) -> UnsafePointer<Element> {
// CHECK-DAG: sil public_external @_TFVSs13UnsafePointer10initializeU__fGS_Q__FT8newvalueQ__T_ : $@cc(method) @thin <T> (@in T, UnsafePointer<T>) -> () {
// CHECK-DAG: sil public_external @_TFSsoi1pU__FT3lhsGVSs13UnsafePointerQ__3rhsSi_GS_Q__ : $@thin <T> (UnsafePointer<T>, Int) -> UnsafePointer<T> {
// CHECK-DAG: sil public_external [transparent] @_TFVSs13UnsafePointerg9subscriptFT1iSi_Q_ : $@cc(method) @thin <T> (@out T, Int, UnsafePointer<T>) -> () {
// CHECK-DAG: sil public_external @_TFVSs10HeapBuffer12__conversionU___fGS_Q_Q0__FT_Bo : $@cc(method) @thin <Value, Element> (@owned HeapBuffer<Value, Element>) -> @owned Builtin.ObjectPointer {
// CHECK-DAG: sil public_external @_TFVSs10HeapBufferCU___fMGS_Q_Q0__FT12storageClassMCSo21HeapBufferStorageBase11initializerQ_8capacitySi_GS_Q_Q0__ : $@thin <Value, Element> (@thick HeapBufferStorageBase.Type, @in Value, Int, @thin HeapBuffer<Value, Element>.Type) -> @owned HeapBuffer<Value, Element> {
// CHECK-DAG: sil public_external @_TFVSs16CountAndCapacityCfMS_FT5countSi8capacitySi20managedByCopyOnWriteSb_S_ : $@thin (Int, Int, Bool, @thin CountAndCapacity.Type) -> CountAndCapacity {
// CHECK-DAG: sil public_external @_TFVSs10HeapBuffer14_elementOffsetU___fMGS_Q_Q0__FT_Si : $@thin <Value, Element> (@thin HeapBuffer<Value, Element>.Type) -> Int {
// CHECK-DAG: sil public_external @_TFVSs10HeapBufferg8_addressGVSs13UnsafePointerVSs4Int8_ : $@cc(method) @thin <Value, Element> (@owned HeapBuffer<Value, Element>) -> UnsafePointer<Int8> {
// CHECK-DAG: sil public_external @_TFSs25_writeLineNumberToConsoleFT4lineSu_T_ : $@thin (UInt) -> () {
// CHECK-DAG: sil public_external @_TFVSs13UnsafePointer6isNullU__fGS_Q__FT_Sb : $@cc(method) @thin <T> (UnsafePointer<T>) -> Bool {
// CHECK-DAG: sil public_external @_TFVSs13UnsafePointerCU__fMGS_Q__FT5valueBp_GS_Q__ : $@thin <T> (Builtin.RawPointer, @thin UnsafePointer<T>.Type) -> UnsafePointer<T> {
// CHECK-DAG: sil public_external [transparent] @_TFVSs13UnsafePointer3getU__fGS_Q__FT_Q_ : $@cc(method) @thin <T> (@out T, UnsafePointer<T>) -> () {
// CHECK-DAG: sil public_external @_TFSs15reinterpretCastU___FT1xQ__Q0_ : $@thin <T, U> (@out U, @in T) -> () {
// CHECK-DAG: sil public_external @_TFSsoi2eeU__FT3lhsGVSs13UnsafePointerQ__3rhsGS_Q___Sb : $@thin <T> (UnsafePointer<T>, UnsafePointer<T>) -> Bool {
// CHECK-DAG: sil public_external @_TFVSs13UnsafePointerCU__fMGS_Q__FT_GS_Q__ : $@thin <T> (@thin UnsafePointer<T>.Type) -> UnsafePointer<T> {
// CHECK-DAG: sil public_external @_TFVSs10HeapBuffer12_valueOffsetU___fMGS_Q_Q0__FT_Si : $@thin <Value, Element> (@thin HeapBuffer<Value, Element>.Type) -> Int {
// CHECK-DAG: sil public_external @_TFSs6sizeofU__FMQ_Si : $@thin <T> (@thick T.Type) -> Int {
// CHECK-DAG: sil public_external @_TFSs7alignofU__FMQ_Si : $@thin <T> (@thick T.Type) -> Int {
// CHECK-DAG: sil public_external @_TFVSs10HeapBufferg6_valueGVSs13UnsafePointerQ__ : $@cc(method) @thin <Value, Element> (@owned HeapBuffer<Value, Element>) -> UnsafePointer<Value> {
// CHECK-DAG: sil public_external @_TFCSo18ArrayBufferStoragecU__fMGS_Q__FT_GS_Q__ : $@cc(method) @thin <T> (@owned ArrayBufferStorage<T>) -> @owned ArrayBufferStorage<T> {
// CHECK-DAG: sil public_external @_TFCSo17HeapBufferStoragecU___fMGS_Q_Q0__FT_GS_Q_Q0__ : $@cc(method) @thin <Value, Element> (@owned HeapBufferStorage<Value, Element>) -> @owned HeapBufferStorage<Value, Element> {
// CHECK-DAG: sil_vtable ArrayBufferStorage {

var a = Array<UInt8>()

var b : UInt8 = 0
a.append(b)
a.append(1)
a.append(2)
a.append(3)
b += 1
