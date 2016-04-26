
@discardableResult
func PyArena_New() -> OpaquePointer!
func PyArena_Free(_ _: OpaquePointer!)
@discardableResult
func PyArena_Malloc(_ _: OpaquePointer!, _ size: Int) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyArena_AddPyObject(_ _: OpaquePointer!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
