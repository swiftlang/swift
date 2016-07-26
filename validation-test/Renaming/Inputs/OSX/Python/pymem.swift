
@discardableResult
func PyMem_Malloc(_ _: Int) -> UnsafeMutablePointer<Void>!
@discardableResult
func PyMem_Realloc(_ _: UnsafeMutablePointer<Void>!, _ _: Int) -> UnsafeMutablePointer<Void>!
func PyMem_Free(_ _: UnsafeMutablePointer<Void>!)
