
var PySet_MINSIZE: Int32 { get }
struct setentry {
  var hash: Int
  var key: UnsafeMutablePointer<PyObject>!
  init()
  init(hash hash: Int, key key: UnsafeMutablePointer<PyObject>!)
}
typealias PySetObject = _setobject
struct _setobject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var fill: Py_ssize_t
  var used: Py_ssize_t
  var mask: Py_ssize_t
  var table: UnsafeMutablePointer<setentry>!
  var lookup: (@convention(c) (UnsafeMutablePointer<PySetObject>!, UnsafeMutablePointer<PyObject>!, Int) -> UnsafeMutablePointer<setentry>!)!
  var smalltable: (setentry, setentry, setentry, setentry, setentry, setentry, setentry, setentry)
  var hash: Int
  var weakreflist: UnsafeMutablePointer<PyObject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, fill fill: Py_ssize_t, used used: Py_ssize_t, mask mask: Py_ssize_t, table table: UnsafeMutablePointer<setentry>!, lookup lookup: (@convention(c) (UnsafeMutablePointer<PySetObject>!, UnsafeMutablePointer<PyObject>!, Int) -> UnsafeMutablePointer<setentry>!)!, smalltable smalltable: (setentry, setentry, setentry, setentry, setentry, setentry, setentry, setentry), hash hash: Int, weakreflist weakreflist: UnsafeMutablePointer<PyObject>!)
}
var PySet_Type: PyTypeObject
var PyFrozenSet_Type: PyTypeObject
@discardableResult
func PySet_New(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyFrozenSet_New(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PySet_Size(_ anyset: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func PySet_Clear(_ set: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PySet_Contains(_ anyset: UnsafeMutablePointer<PyObject>!, _ key: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PySet_Discard(_ set: UnsafeMutablePointer<PyObject>!, _ key: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PySet_Add(_ set: UnsafeMutablePointer<PyObject>!, _ key: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func _PySet_Next(_ set: UnsafeMutablePointer<PyObject>!, _ pos: UnsafeMutablePointer<Py_ssize_t>!, _ key: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!) -> Int32
@discardableResult
func _PySet_NextEntry(_ set: UnsafeMutablePointer<PyObject>!, _ pos: UnsafeMutablePointer<Py_ssize_t>!, _ key: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ hash: UnsafeMutablePointer<Int>!) -> Int32
@discardableResult
func PySet_Pop(_ set: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PySet_Update(_ set: UnsafeMutablePointer<PyObject>!, _ iterable: UnsafeMutablePointer<PyObject>!) -> Int32
