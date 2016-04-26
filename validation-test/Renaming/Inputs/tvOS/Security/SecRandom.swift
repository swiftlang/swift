
typealias SecRandomRef = OpaquePointer
@available(tvOS 2.0, *)
let kSecRandomDefault: SecRandomRef
@available(tvOS 2.0, *)
@discardableResult
func SecRandomCopyBytes(_ rnd: SecRandomRef?, _ count: Int, _ bytes: UnsafeMutablePointer<UInt8>) -> Int32
