
@discardableResult
func NSPageSize() -> Int
@discardableResult
func NSLogPageSize() -> Int
@discardableResult
func NSRoundUpToMultipleOfPageSize(_ bytes: Int) -> Int
@discardableResult
func NSRoundDownToMultipleOfPageSize(_ bytes: Int) -> Int
@discardableResult
func NSAllocateMemoryPages(_ bytes: Int) -> UnsafeMutablePointer<Void>
func NSDeallocateMemoryPages(_ ptr: UnsafeMutablePointer<Void>, _ bytes: Int)
func NSCopyMemoryPages(_ source: UnsafePointer<Void>, _ dest: UnsafeMutablePointer<Void>, _ bytes: Int)
@available(watchOS, introduced: 2.0, deprecated: 2.0)
@discardableResult
func NSRealMemoryAvailable() -> Int
