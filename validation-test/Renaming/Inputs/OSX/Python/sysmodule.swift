
@discardableResult
func PySys_GetObject(_ _: UnsafeMutablePointer<Int8>!) -> UnsafeMutablePointer<PyObject>!
@discardableResult
func PySys_SetObject(_ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<PyObject>!) -> Int32
@discardableResult
func PySys_GetFile(_ _: UnsafeMutablePointer<Int8>!, _ _: UnsafeMutablePointer<FILE>!) -> UnsafeMutablePointer<FILE>!
func PySys_SetArgv(_ _: Int32, _ _: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!)
func PySys_SetArgvEx(_ _: Int32, _ _: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!, _ _: Int32)
func PySys_SetPath(_ _: UnsafeMutablePointer<Int8>!)
func PySys_ResetWarnOptions()
func PySys_AddWarnOption(_ _: UnsafeMutablePointer<Int8>!)
@discardableResult
func PySys_HasWarnOptions() -> Int32
@discardableResult
func _PySys_GetSizeOf(_ _: UnsafeMutablePointer<PyObject>!) -> Int
