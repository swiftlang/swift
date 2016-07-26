
var PyCapsule_Type: PyTypeObject
typealias PyCapsule_Destructor = @convention(c) (UnsafeMutablePointer<PyObject>!) -> Void
@discardableResult
func PyCapsule_New(_ pointer: UnsafeMutablePointer<Void>!, _ name: UnsafePointer<Int8>!, _ destructor: PyCapsule_Destructor!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PyCapsule_GetPointer(_ capsule: UnsafeMutablePointer<PyObject>!, _ name: UnsafePointer<Int8>!) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyCapsule_GetDestructor(_ capsule: UnsafeMutablePointer<PyObject>!) -> PyCapsule_Destructor!
@discardableResult
func PyCapsule_GetName(_ capsule: UnsafeMutablePointer<PyObject>!) -> UnsafePointer<Int8>!
@discardableResult
func PyCapsule_GetContext(_ capsule: UnsafeMutablePointer<PyObject>!) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyCapsule_IsValid(_ capsule: UnsafeMutablePointer<PyObject>!, _ name: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyCapsule_SetPointer(_ capsule: UnsafeMutablePointer<PyObject>!, _ pointer: UnsafeMutablePointer<Void>!) -> Int32
@discardableResult
func PyCapsule_SetDestructor(_ capsule: UnsafeMutablePointer<PyObject>!, _ destructor: PyCapsule_Destructor!) -> Int32
@discardableResult
func PyCapsule_SetName(_ capsule: UnsafeMutablePointer<PyObject>!, _ name: UnsafePointer<Int8>!) -> Int32
@discardableResult
func PyCapsule_SetContext(_ capsule: UnsafeMutablePointer<PyObject>!, _ context: UnsafeMutablePointer<Void>!) -> Int32
@discardableResult
func PyCapsule_Import(_ name: UnsafePointer<Int8>!, _ no_block: Int32) -> UnsafeMutablePointer<Void>!
