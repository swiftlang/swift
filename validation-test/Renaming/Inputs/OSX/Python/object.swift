
struct _object {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!)
}
typealias PyObject = _object
struct PyVarObject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var ob_size: Py_ssize_t
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, ob_size ob_size: Py_ssize_t)
}
typealias unaryfunc = @convention(c) (UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias binaryfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias ternaryfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias inquiry = @convention(c) (UnsafeMutablePointer<PyObject>!) -> Int32
typealias lenfunc = @convention(c) (UnsafeMutablePointer<PyObject>!) -> Py_ssize_t
typealias coercion = @convention(c) (UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!) -> Int32
@available(*, deprecated)
typealias intargfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, Int32) -> UnsafeMutablePointer<PyObject>!
@available(*, deprecated)
typealias intintargfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, Int32, Int32) -> UnsafeMutablePointer<PyObject>!
typealias ssizeargfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
typealias ssizessizeargfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, Py_ssize_t, Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
typealias intobjargproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Int32, UnsafeMutablePointer<PyObject>!) -> Int32
typealias intintobjargproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Int32, Int32, UnsafeMutablePointer<PyObject>!) -> Int32
typealias ssizeobjargproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Py_ssize_t, UnsafeMutablePointer<PyObject>!) -> Int32
typealias ssizessizeobjargproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Py_ssize_t, Py_ssize_t, UnsafeMutablePointer<PyObject>!) -> Int32
typealias objobjargproc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> Int32
typealias getreadbufferproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!) -> Int32
typealias getwritebufferproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!) -> Int32
typealias getsegcountproc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Int32>!) -> Int32
typealias getcharbufferproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Int32, UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!) -> Int32
typealias readbufferproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Py_ssize_t, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!) -> Py_ssize_t
typealias writebufferproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Py_ssize_t, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!) -> Py_ssize_t
typealias segcountproc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Py_ssize_t>!) -> Py_ssize_t
typealias charbufferproc = @convention(c) (UnsafeMutablePointer<PyObject>!, Py_ssize_t, UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!) -> Py_ssize_t
struct bufferinfo {
  var buf: UnsafeMutablePointer<Void>!
  var obj: UnsafeMutablePointer<PyObject>!
  var len: Py_ssize_t
  var itemsize: Py_ssize_t
  var readonly: Int32
  var ndim: Int32
  var format: UnsafeMutablePointer<Int8>!
  var shape: UnsafeMutablePointer<Py_ssize_t>!
  var strides: UnsafeMutablePointer<Py_ssize_t>!
  var suboffsets: UnsafeMutablePointer<Py_ssize_t>!
  var smalltable: (Py_ssize_t, Py_ssize_t)
  var `internal`: UnsafeMutablePointer<Void>!
  init()
  init(buf buf: UnsafeMutablePointer<Void>!, obj obj: UnsafeMutablePointer<PyObject>!, len len: Py_ssize_t, itemsize itemsize: Py_ssize_t, readonly readonly: Int32, ndim ndim: Int32, format format: UnsafeMutablePointer<Int8>!, shape shape: UnsafeMutablePointer<Py_ssize_t>!, strides strides: UnsafeMutablePointer<Py_ssize_t>!, suboffsets suboffsets: UnsafeMutablePointer<Py_ssize_t>!, smalltable smalltable: (Py_ssize_t, Py_ssize_t), internal internal: UnsafeMutablePointer<Void>!)
}
typealias Py_buffer = bufferinfo
typealias getbufferproc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Py_buffer>!, Int32) -> Int32
typealias releasebufferproc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Py_buffer>!) -> Void
var PyBUF_SIMPLE: Int32 { get }
var PyBUF_WRITABLE: Int32 { get }
var PyBUF_WRITEABLE: Int32 { get }
var PyBUF_FORMAT: Int32 { get }
var PyBUF_ND: Int32 { get }
var PyBUF_CONTIG_RO: Int32 { get }
var PyBUF_READ: Int32 { get }
var PyBUF_WRITE: Int32 { get }
var PyBUF_SHADOW: Int32 { get }
typealias objobjproc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> Int32
typealias visitproc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Void>!) -> Int32
typealias traverseproc = @convention(c) (UnsafeMutablePointer<PyObject>!, visitproc!, UnsafeMutablePointer<Void>!) -> Int32
struct PyNumberMethods {
  var nb_add: binaryfunc!
  var nb_subtract: binaryfunc!
  var nb_multiply: binaryfunc!
  var nb_divide: binaryfunc!
  var nb_remainder: binaryfunc!
  var nb_divmod: binaryfunc!
  var nb_power: ternaryfunc!
  var nb_negative: unaryfunc!
  var nb_positive: unaryfunc!
  var nb_absolute: unaryfunc!
  var nb_nonzero: inquiry!
  var nb_invert: unaryfunc!
  var nb_lshift: binaryfunc!
  var nb_rshift: binaryfunc!
  var nb_and: binaryfunc!
  var nb_xor: binaryfunc!
  var nb_or: binaryfunc!
  var nb_coerce: coercion!
  var nb_int: unaryfunc!
  var nb_long: unaryfunc!
  var nb_float: unaryfunc!
  var nb_oct: unaryfunc!
  var nb_hex: unaryfunc!
  var nb_inplace_add: binaryfunc!
  var nb_inplace_subtract: binaryfunc!
  var nb_inplace_multiply: binaryfunc!
  var nb_inplace_divide: binaryfunc!
  var nb_inplace_remainder: binaryfunc!
  var nb_inplace_power: ternaryfunc!
  var nb_inplace_lshift: binaryfunc!
  var nb_inplace_rshift: binaryfunc!
  var nb_inplace_and: binaryfunc!
  var nb_inplace_xor: binaryfunc!
  var nb_inplace_or: binaryfunc!
  var nb_floor_divide: binaryfunc!
  var nb_true_divide: binaryfunc!
  var nb_inplace_floor_divide: binaryfunc!
  var nb_inplace_true_divide: binaryfunc!
  var nb_index: unaryfunc!
  init()
  init(nb_add nb_add: binaryfunc!, nb_subtract nb_subtract: binaryfunc!, nb_multiply nb_multiply: binaryfunc!, nb_divide nb_divide: binaryfunc!, nb_remainder nb_remainder: binaryfunc!, nb_divmod nb_divmod: binaryfunc!, nb_power nb_power: ternaryfunc!, nb_negative nb_negative: unaryfunc!, nb_positive nb_positive: unaryfunc!, nb_absolute nb_absolute: unaryfunc!, nb_nonzero nb_nonzero: inquiry!, nb_invert nb_invert: unaryfunc!, nb_lshift nb_lshift: binaryfunc!, nb_rshift nb_rshift: binaryfunc!, nb_and nb_and: binaryfunc!, nb_xor nb_xor: binaryfunc!, nb_or nb_or: binaryfunc!, nb_coerce nb_coerce: coercion!, nb_int nb_int: unaryfunc!, nb_long nb_long: unaryfunc!, nb_float nb_float: unaryfunc!, nb_oct nb_oct: unaryfunc!, nb_hex nb_hex: unaryfunc!, nb_inplace_add nb_inplace_add: binaryfunc!, nb_inplace_subtract nb_inplace_subtract: binaryfunc!, nb_inplace_multiply nb_inplace_multiply: binaryfunc!, nb_inplace_divide nb_inplace_divide: binaryfunc!, nb_inplace_remainder nb_inplace_remainder: binaryfunc!, nb_inplace_power nb_inplace_power: ternaryfunc!, nb_inplace_lshift nb_inplace_lshift: binaryfunc!, nb_inplace_rshift nb_inplace_rshift: binaryfunc!, nb_inplace_and nb_inplace_and: binaryfunc!, nb_inplace_xor nb_inplace_xor: binaryfunc!, nb_inplace_or nb_inplace_or: binaryfunc!, nb_floor_divide nb_floor_divide: binaryfunc!, nb_true_divide nb_true_divide: binaryfunc!, nb_inplace_floor_divide nb_inplace_floor_divide: binaryfunc!, nb_inplace_true_divide nb_inplace_true_divide: binaryfunc!, nb_index nb_index: unaryfunc!)
}
struct PySequenceMethods {
  var sq_length: lenfunc!
  var sq_concat: binaryfunc!
  var sq_repeat: ssizeargfunc!
  var sq_item: ssizeargfunc!
  var sq_slice: ssizessizeargfunc!
  var sq_ass_item: ssizeobjargproc!
  var sq_ass_slice: ssizessizeobjargproc!
  var sq_contains: objobjproc!
  var sq_inplace_concat: binaryfunc!
  var sq_inplace_repeat: ssizeargfunc!
  init()
  init(sq_length sq_length: lenfunc!, sq_concat sq_concat: binaryfunc!, sq_repeat sq_repeat: ssizeargfunc!, sq_item sq_item: ssizeargfunc!, sq_slice sq_slice: ssizessizeargfunc!, sq_ass_item sq_ass_item: ssizeobjargproc!, sq_ass_slice sq_ass_slice: ssizessizeobjargproc!, sq_contains sq_contains: objobjproc!, sq_inplace_concat sq_inplace_concat: binaryfunc!, sq_inplace_repeat sq_inplace_repeat: ssizeargfunc!)
}
struct PyMappingMethods {
  var mp_length: lenfunc!
  var mp_subscript: binaryfunc!
  var mp_ass_subscript: objobjargproc!
  init()
  init(mp_length mp_length: lenfunc!, mp_subscript mp_subscript: binaryfunc!, mp_ass_subscript mp_ass_subscript: objobjargproc!)
}
struct PyBufferProcs {
  var bf_getreadbuffer: readbufferproc!
  var bf_getwritebuffer: writebufferproc!
  var bf_getsegcount: segcountproc!
  var bf_getcharbuffer: charbufferproc!
  var bf_getbuffer: getbufferproc!
  var bf_releasebuffer: releasebufferproc!
  init()
  init(bf_getreadbuffer bf_getreadbuffer: readbufferproc!, bf_getwritebuffer bf_getwritebuffer: writebufferproc!, bf_getsegcount bf_getsegcount: segcountproc!, bf_getcharbuffer bf_getcharbuffer: charbufferproc!, bf_getbuffer bf_getbuffer: getbufferproc!, bf_releasebuffer bf_releasebuffer: releasebufferproc!)
}
typealias freefunc = @convention(c) (UnsafeMutablePointer<Void>!) -> Void
typealias destructor = @convention(c) (UnsafeMutablePointer<PyObject>!) -> Void
typealias printfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<FILE>!, Int32) -> Int32
typealias getattrfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
typealias getattrofunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias setattrfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<Int8>!, UnsafeMutablePointer<PyObject>!) -> Int32
typealias setattrofunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> Int32
typealias cmpfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> Int32
typealias reprfunc = @convention(c) (UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias hashfunc = @convention(c) (UnsafeMutablePointer<PyObject>!) -> Int
typealias richcmpfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, Int32) -> UnsafeMutablePointer<PyObject>!
typealias getiterfunc = @convention(c) (UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias iternextfunc = @convention(c) (UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias descrgetfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias descrsetfunc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> Int32
typealias initproc = @convention(c) (UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> Int32
typealias newfunc = @convention(c) (UnsafeMutablePointer<_typeobject>!, UnsafeMutablePointer<PyObject>!, UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
typealias allocfunc = @convention(c) (UnsafeMutablePointer<_typeobject>!, Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
struct _typeobject {
  var ob_refcnt: Py_ssize_t
  var ob_type: UnsafeMutablePointer<_typeobject>!
  var ob_size: Py_ssize_t
  var tp_name: UnsafePointer<Int8>!
  var tp_basicsize: Py_ssize_t
  var tp_itemsize: Py_ssize_t
  var tp_dealloc: destructor!
  var tp_print: printfunc!
  var tp_getattr: getattrfunc!
  var tp_setattr: setattrfunc!
  var tp_compare: cmpfunc!
  var tp_repr: reprfunc!
  var tp_as_number: UnsafeMutablePointer<PyNumberMethods>!
  var tp_as_sequence: UnsafeMutablePointer<PySequenceMethods>!
  var tp_as_mapping: UnsafeMutablePointer<PyMappingMethods>!
  var tp_hash: hashfunc!
  var tp_call: ternaryfunc!
  var tp_str: reprfunc!
  var tp_getattro: getattrofunc!
  var tp_setattro: setattrofunc!
  var tp_as_buffer: UnsafeMutablePointer<PyBufferProcs>!
  var tp_flags: Int
  var tp_doc: UnsafePointer<Int8>!
  var tp_traverse: traverseproc!
  var tp_clear: inquiry!
  var tp_richcompare: richcmpfunc!
  var tp_weaklistoffset: Py_ssize_t
  var tp_iter: getiterfunc!
  var tp_iternext: iternextfunc!
  var tp_methods: UnsafeMutablePointer<PyMethodDef>!
  var tp_members: UnsafeMutablePointer<PyMemberDef>!
  var tp_getset: UnsafeMutablePointer<PyGetSetDef>!
  var tp_base: UnsafeMutablePointer<_typeobject>!
  var tp_dict: UnsafeMutablePointer<PyObject>!
  var tp_descr_get: descrgetfunc!
  var tp_descr_set: descrsetfunc!
  var tp_dictoffset: Py_ssize_t
  var tp_init: initproc!
  var tp_alloc: allocfunc!
  var tp_new: newfunc!
  var tp_free: freefunc!
  var tp_is_gc: inquiry!
  var tp_bases: UnsafeMutablePointer<PyObject>!
  var tp_mro: UnsafeMutablePointer<PyObject>!
  var tp_cache: UnsafeMutablePointer<PyObject>!
  var tp_subclasses: UnsafeMutablePointer<PyObject>!
  var tp_weaklist: UnsafeMutablePointer<PyObject>!
  var tp_del: destructor!
  var tp_version_tag: UInt32
  init()
  init(ob_refcnt ob_refcnt: Py_ssize_t, ob_type ob_type: UnsafeMutablePointer<_typeobject>!, ob_size ob_size: Py_ssize_t, tp_name tp_name: UnsafePointer<Int8>!, tp_basicsize tp_basicsize: Py_ssize_t, tp_itemsize tp_itemsize: Py_ssize_t, tp_dealloc tp_dealloc: destructor!, tp_print tp_print: printfunc!, tp_getattr tp_getattr: getattrfunc!, tp_setattr tp_setattr: setattrfunc!, tp_compare tp_compare: cmpfunc!, tp_repr tp_repr: reprfunc!, tp_as_number tp_as_number: UnsafeMutablePointer<PyNumberMethods>!, tp_as_sequence tp_as_sequence: UnsafeMutablePointer<PySequenceMethods>!, tp_as_mapping tp_as_mapping: UnsafeMutablePointer<PyMappingMethods>!, tp_hash tp_hash: hashfunc!, tp_call tp_call: ternaryfunc!, tp_str tp_str: reprfunc!, tp_getattro tp_getattro: getattrofunc!, tp_setattro tp_setattro: setattrofunc!, tp_as_buffer tp_as_buffer: UnsafeMutablePointer<PyBufferProcs>!, tp_flags tp_flags: Int, tp_doc tp_doc: UnsafePointer<Int8>!, tp_traverse tp_traverse: traverseproc!, tp_clear tp_clear: inquiry!, tp_richcompare tp_richcompare: richcmpfunc!, tp_weaklistoffset tp_weaklistoffset: Py_ssize_t, tp_iter tp_iter: getiterfunc!, tp_iternext tp_iternext: iternextfunc!, tp_methods tp_methods: UnsafeMutablePointer<PyMethodDef>!, tp_members tp_members: UnsafeMutablePointer<PyMemberDef>!, tp_getset tp_getset: UnsafeMutablePointer<PyGetSetDef>!, tp_base tp_base: UnsafeMutablePointer<_typeobject>!, tp_dict tp_dict: UnsafeMutablePointer<PyObject>!, tp_descr_get tp_descr_get: descrgetfunc!, tp_descr_set tp_descr_set: descrsetfunc!, tp_dictoffset tp_dictoffset: Py_ssize_t, tp_init tp_init: initproc!, tp_alloc tp_alloc: allocfunc!, tp_new tp_new: newfunc!, tp_free tp_free: freefunc!, tp_is_gc tp_is_gc: inquiry!, tp_bases tp_bases: UnsafeMutablePointer<PyObject>!, tp_mro tp_mro: UnsafeMutablePointer<PyObject>!, tp_cache tp_cache: UnsafeMutablePointer<PyObject>!, tp_subclasses tp_subclasses: UnsafeMutablePointer<PyObject>!, tp_weaklist tp_weaklist: UnsafeMutablePointer<PyObject>!, tp_del tp_del: destructor!, tp_version_tag tp_version_tag: UInt32)
}
typealias PyTypeObject = _typeobject
struct _heaptypeobject {
  var ht_type: PyTypeObject
  var as_number: PyNumberMethods
  var as_mapping: PyMappingMethods
  var as_sequence: PySequenceMethods
  var as_buffer: PyBufferProcs
  var ht_name: UnsafeMutablePointer<PyObject>!
  var ht_slots: UnsafeMutablePointer<PyObject>!
  init()
  init(ht_type ht_type: PyTypeObject, as_number as_number: PyNumberMethods, as_mapping as_mapping: PyMappingMethods, as_sequence as_sequence: PySequenceMethods, as_buffer as_buffer: PyBufferProcs, ht_name ht_name: UnsafeMutablePointer<PyObject>!, ht_slots ht_slots: UnsafeMutablePointer<PyObject>!)
}
typealias PyHeapTypeObject = _heaptypeobject
@discardableResult
func PyType_IsSubtype(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: UnsafeMutablePointer<PyTypeObject>!) -> Int32
var PyType_Type: PyTypeObject
var PyBaseObject_Type: PyTypeObject
var PySuper_Type: PyTypeObject
@discardableResult
func PyType_Ready(_ _: UnsafeMutablePointer<PyTypeObject>!) -> Int32
@discardableResult
func PyType_GenericAlloc(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: Py_ssize_t) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyType_GenericNew(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyType_Lookup(_ _: UnsafeMutablePointer<PyTypeObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyObject_LookupSpecial(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyType_ClearCache() -> UInt32
func PyType_Modified(_ _: UnsafeMutablePointer<PyTypeObject>!)
@discardableResult
func PyObject_Print(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<FILE>!, _ _: Int32) -> Int32
func _PyObject_Dump(_ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func PyObject_Repr(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyObject_Str(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_Str(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_Unicode(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_Compare(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyObject_RichCompare(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: Int32) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_RichCompareBool(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: Int32) -> Int32
@discardableResult
func PyObject_GetAttrString(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_SetAttrString(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyObject_HasAttrString(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyObject_GetAttr(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_SetAttr(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyObject_HasAttr(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func _PyObject_GetDictPtr(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!
@discardableResult
func PyObject_SelfIter(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyObject_NextNotImplemented(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_GenericGetAttr(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyObject_GenericSetAttr(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyObject_Hash(_ _: UnsafeMutablePointer<PyObject>!) -> Int
@discardableResult
func PyObject_HashNotImplemented(_ _: UnsafeMutablePointer<PyObject>!) -> Int
@discardableResult
func PyObject_IsTrue(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyObject_Not(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyCallable_Check(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyNumber_Coerce(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!) -> Int32
@discardableResult
func PyNumber_CoerceEx(_ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!, _ _: UnsafeMutablePointer<UnsafeMutablePointer<PyObject>?>!) -> Int32
func PyObject_ClearWeakRefs(_ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func _PyObject_SlotCompare(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func _PyObject_GenericGetAttrWithDict(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func _PyObject_GenericSetAttrWithDict(_ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PyObject_Dir(_ _: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func Py_ReprEnter(_ _: UnsafeMutablePointer<PyObject>!) -> Int32
func Py_ReprLeave(_ _: UnsafeMutablePointer<PyObject>!)
@discardableResult
func _Py_HashDouble(_ _: Double) -> Int
@discardableResult
func _Py_HashPointer(_ _: UnsafeMutablePointer<Void>!) -> Int
struct _Py_HashSecret_t {
  var prefix: Int
  var suffix: Int
  init()
  init(prefix prefix: Int, suffix suffix: Int)
}
var _Py_HashSecret: _Py_HashSecret_t
var Py_PRINT_RAW: Int32 { get }
var Py_TPFLAGS_HAVE_GETCHARBUFFER: Int { get }
var Py_TPFLAGS_HAVE_SEQUENCE_IN: Int { get }
var Py_TPFLAGS_GC: Int32 { get }
var Py_TPFLAGS_HAVE_INPLACEOPS: Int { get }
var Py_TPFLAGS_CHECKTYPES: Int { get }
var Py_TPFLAGS_HAVE_RICHCOMPARE: Int { get }
var Py_TPFLAGS_HAVE_WEAKREFS: Int { get }
var Py_TPFLAGS_HAVE_ITER: Int { get }
var Py_TPFLAGS_HAVE_CLASS: Int { get }
var Py_TPFLAGS_HEAPTYPE: Int { get }
var Py_TPFLAGS_BASETYPE: Int { get }
var Py_TPFLAGS_READY: Int { get }
var Py_TPFLAGS_READYING: Int { get }
var Py_TPFLAGS_HAVE_GC: Int { get }
var Py_TPFLAGS_HAVE_STACKLESS_EXTENSION: Int32 { get }
var Py_TPFLAGS_HAVE_INDEX: Int { get }
var Py_TPFLAGS_HAVE_VERSION_TAG: Int { get }
var Py_TPFLAGS_VALID_VERSION_TAG: Int { get }
var Py_TPFLAGS_IS_ABSTRACT: Int { get }
var Py_TPFLAGS_HAVE_NEWBUFFER: Int { get }
var Py_TPFLAGS_INT_SUBCLASS: Int { get }
var Py_TPFLAGS_LONG_SUBCLASS: Int { get }
var Py_TPFLAGS_LIST_SUBCLASS: Int { get }
var Py_TPFLAGS_TUPLE_SUBCLASS: Int { get }
var Py_TPFLAGS_STRING_SUBCLASS: Int { get }
var Py_TPFLAGS_UNICODE_SUBCLASS: Int { get }
var Py_TPFLAGS_DICT_SUBCLASS: Int { get }
var Py_TPFLAGS_BASE_EXC_SUBCLASS: Int { get }
var Py_TPFLAGS_TYPE_SUBCLASS: Int { get }
func Py_IncRef(_ _: UnsafeMutablePointer<PyObject>!)
func Py_DecRef(_ _: UnsafeMutablePointer<PyObject>!)
var _Py_NoneStruct: PyObject
var _Py_NotImplementedStruct: PyObject
var Py_LT: Int32 { get }
var Py_LE: Int32 { get }
var Py_EQ: Int32 { get }
var Py_NE: Int32 { get }
var Py_GT: Int32 { get }
var Py_GE: Int32 { get }
func _PyTrash_deposit_object(_ _: UnsafeMutablePointer<PyObject>!)
func _PyTrash_destroy_chain()
var _PyTrash_delete_nesting: Int32
var _PyTrash_delete_later: UnsafeMutablePointer<PyObject>!
func _PyTrash_thread_deposit_object(_ _: UnsafeMutablePointer<PyObject>!)
func _PyTrash_thread_destroy_chain()
var PyTrash_UNWIND_LEVEL: Int32 { get }
