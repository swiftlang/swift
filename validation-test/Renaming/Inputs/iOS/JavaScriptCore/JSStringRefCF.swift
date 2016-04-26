
@discardableResult
func JSStringCreateWithCFString(_ string: CFString!) -> JSStringRef!
@discardableResult
func JSStringCopyCFString(_ alloc: CFAllocator!, _ string: JSStringRef!) -> CFString!
