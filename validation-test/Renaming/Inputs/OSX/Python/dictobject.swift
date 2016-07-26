
var PyDict_MINSIZE: Int32 { get }
struct PyDictEntry {
  var me_hash: Py_ssize_t
  var me_key: UnsafeMutablePointer<PyObject>!
  var me_value: UnsafeMutablePointer<PyObject>!
  init()
  init(me_hash me_hash: Py_ssize_t, me_key me_key: UnsafeMutablePointer<PyObject>!, me_value me_value: UnsafeMutablePointer<PyObject>!)
}
typealias PyDictObject = _dictobject
struct _dictobject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var ma_fill: Py_ssize_t
  var ma_used: Py_ssize_t
  var ma_mask: Py_ssize_t
  var ma_table: UnsafeMutablePointer<PyDictEntry>!
  var ma_lookup: (@convention(c) (UnsafeMutablePointer<PyDictObject>!, UnsafeMutablePointer<PyObject>!, Int) -> UnsafeMutablePointer<PyDictEntry>!)!
  var ma_smalltable: (PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry)
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, ma_fill ma_fill: Py_ssize_t, ma_used ma_used: Py_ssize_t, ma_mask ma_mask: Py_ssize_t, ma_table ma_table: UnsafeMutablePointer<PyDictEntry>!, ma_lookup ma_lookup: (@convention(c) (UnsafeMutablePointer<PyDictObject>!, UnsafeMutablePointer<PyObject>!, Int) -> UnsafeMutablePointer<PyDictEntry>!)!, ma_smalltable ma_smalltable: (PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry, PyDictEntry))
}
var PyDict_Type: PyTypeObject
var PyDictIterKey_Type: PyTypeObject
var PyDictIterValue_Type: PyTypeObject
var PyDictIterItem_Type: PyTypeObject
var PyDictKeys_Type: PyTypeObject
var PyDictItems_Type: PyTypeObject
var PyDictValues_Type: PyTypeObject
@discardableResult
func PyDict_New() -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDict_GetItem(_ mp: UnsafeMutablePointer<PyObject>!, _ key: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDict_SetItem(_ mp: UnsafeMutablePointer<PyObject>!, _ key: UnsafeMutablePointer<PyObject>!, _ item: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyDict_DelItem(_ mp: UnsafeMutablePointer<PyObject>!, _ key: UnsafeMutablePointer<PyObject>!) -> Int32
func PyDict_Clear(_ mp: UnsafeMutablePointer<PyObject>!)
@discardableResult
func PyDict_Next(_ mp: UnsafeMutablePointer<PyObject>!, _ pos: UnsafeMutablePointer<Py_ssize_t>!, _ key: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ value: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!) -> Int32
@discardableResult
func _PyDict_Next(_ mp: UnsafeMutablePointer<PyObject>!, _ pos: UnsafeMutablePointer<Py_ssize_t>!, _ key: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ value: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ hash: UnsafeMutablePointer<Int>!) -> Int32
@discardableResult
func PyDict_Keys(_ mp: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDict_Values(_ mp: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDict_Items(_ mp: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDict_Size(_ mp: UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
@discardableResult
func PyDict_Copy(_ mp: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDict_Contains(_ mp: UnsafeMutablePointer<PyObject>!, _ key: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func _PyDict_Contains(_ mp: UnsafeMutablePointer<PyObject>!, _ key: UnsafeMutablePointer<PyObject>!, _ hash: Int) -> Int32
@discardableResult
func _PyDict_NewPresized(_ minused: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
func _PyDict_MaybeUntrack(_ mp: UnsafeMutablePointer<PyObject>!)
@discardableResult
func PyDict_Update(_ mp: UnsafeMutablePointer<PyObject>!, _ other: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyDict_Merge(_ mp: UnsafeMutablePointer<PyObject>!, _ other: UnsafeMutablePointer<PyObject>!, _ override: Int32) -> Int32
@discardableResult
func PyDict_MergeFromSeq2(_ d: UnsafeMutablePointer<PyObject>!, _ seq2: UnsafeMutablePointer<PyObject>!, _ override: Int32) -> Int32
@discardableResult
func PyDict_GetItemString(_ dp: UnsafeMutablePointer<PyObject>!, _ key: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyDict_SetItemString(_ dp: UnsafeMutablePointer<PyObject>!, _ key: UnsafePointer<Int8>!, _ item: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyDict_DelItemString(_ dp: UnsafeMutablePointer<PyObject>!, _ key: UnsafePointer<Int8>!) -> Int32
