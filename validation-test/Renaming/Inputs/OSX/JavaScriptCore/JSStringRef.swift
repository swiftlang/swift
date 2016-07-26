
typealias JSChar = UInt16
@discardableResult
func JSStringCreateWithCharacters(_ chars: UnsafePointer<JSChar>!, _ numChars: Int) -> JSStringRef!
@discardableResult
func JSStringCreateWithUTF8CString(_ string: UnsafePointer<Int8>!) -> JSStringRef!
@discardableResult
func JSStringRetain(_ string: JSStringRef!) -> JSStringRef!
func JSStringRelease(_ string: JSStringRef!)
@discardableResult
func JSStringGetLength(_ string: JSStringRef!) -> Int
@discardableResult
func JSStringGetCharactersPtr(_ string: JSStringRef!) -> UnsafePointer<JSChar>!
@discardableResult
func JSStringGetMaximumUTF8CStringSize(_ string: JSStringRef!) -> Int
@discardableResult
func JSStringGetUTF8CString(_ string: JSStringRef!, _ buffer: UnsafeMutablePointer<Int8>!, _ bufferSize: Int) -> Int
@discardableResult
func JSStringIsEqual(_ a: JSStringRef!, _ b: JSStringRef!) -> Bool
@discardableResult
func JSStringIsEqualToUTF8CString(_ a: JSStringRef!, _ b: UnsafePointer<Int8>!) -> Bool
