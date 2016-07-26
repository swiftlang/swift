
@discardableResult
func PyObject_Malloc(_ _: Int) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyObject_Realloc(_ _: UnsafeMutablePointer<Void>!, _ _: Int) -> UnsafeMutablePointer<Void>!
func PyObject_Free(_ _: UnsafeMutablePointer<Void>!)
@discardableResult
func PyObject_Init(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyTypeObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_InitVar(_ _: UnsafeMutablePointer<PyVarObject>!, _ _: UnsafeMutablePointer<PyTypeObject>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyVarObject>!
@discardableResult
func _PyObject_New(_ _: UnsafeMutablePointer<PyTypeObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyObject_NewVar(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyVarObject>!
@discardableResult
func PyGC_Collect() -> Py_ssize_t
@discardableResult
func _PyObject_GC_Resize(_ _: UnsafeMutablePointer<PyVarObject>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyVarObject>!
struct _gc_head {
  struct __Unnamed_struct_gc {
    var gc_next: UnsafeMutablePointer<_gc_head>!
    var gc_prev: UnsafeMutablePointer<_gc_head>!
    var gc_refs: Py_ssize_t
    init()
    init(gc_next gc_next: UnsafeMutablePointer<_gc_head>!, gc_prev gc_prev: UnsafeMutablePointer<_gc_head>!, gc_refs gc_refs: Py_ssize_t)
  }
  var gc: _gc_head.__Unnamed_struct_gc
  init(gc gc: _gc_head.__Unnamed_struct_gc)
  init()
}
typealias PyGC_Head = _gc_head
var _PyGC_generation0: UnsafeMutablePointer<PyGC_Head>!
var _PyGC_REFS_UNTRACKED: Int32 { get }
var _PyGC_REFS_REACHABLE: Int32 { get }
var _PyGC_REFS_TENTATIVELY_UNREACHABLE: Int32 { get }
@discardableResult
func _PyObject_GC_Malloc(_ _: Int) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyObject_GC_New(_ _: UnsafeMutablePointer<PyTypeObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyObject_GC_NewVar(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyVarObject>!
func PyObject_GC_Track(_ _: UnsafeMutablePointer<Void>!)
func PyObject_GC_UnTrack(_ _: UnsafeMutablePointer<Void>!)
func PyObject_GC_Del(_ _: UnsafeMutablePointer<Void>!)
var PyGC_HEAD_SIZE: Int32 { get }
