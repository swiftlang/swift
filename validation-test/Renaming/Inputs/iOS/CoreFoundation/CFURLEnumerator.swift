
var __COREFOUNDATION_CFURLENUMERATOR__: Int32 { get }
class CFURLEnumerator {
}
@available(iOS 4.0, *)
@discardableResult
func CFURLEnumeratorGetTypeID() -> CFTypeID
struct CFURLEnumeratorOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var descendRecursively: CFURLEnumeratorOptions { get }
  static var skipInvisibles: CFURLEnumeratorOptions { get }
  static var generateFileReferenceURLs: CFURLEnumeratorOptions { get }
  static var skipPackageContents: CFURLEnumeratorOptions { get }
  static var includeDirectoriesPreOrder: CFURLEnumeratorOptions { get }
  static var includeDirectoriesPostOrder: CFURLEnumeratorOptions { get }
}
@available(iOS 4.0, *)
@discardableResult
func CFURLEnumeratorCreateForDirectoryURL(_ alloc: CFAllocator!, _ directoryURL: CFURL!, _ option: CFURLEnumeratorOptions, _ propertyKeys: CFArray!) -> CFURLEnumerator!
@available(iOS 4.0, *)
@discardableResult
func CFURLEnumeratorCreateForMountedVolumes(_ alloc: CFAllocator!, _ option: CFURLEnumeratorOptions, _ propertyKeys: CFArray!) -> CFURLEnumerator!
enum CFURLEnumeratorResult : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case success
  case end
  case error
  case directoryPostOrderSuccess
}
@available(iOS 4.0, *)
@discardableResult
func CFURLEnumeratorGetNextURL(_ enumerator: CFURLEnumerator!, _ url: UnsafeMutablePointer<Unmanaged<CFURL>?>!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> CFURLEnumeratorResult
@available(iOS 4.0, *)
func CFURLEnumeratorSkipDescendents(_ enumerator: CFURLEnumerator!)
@available(iOS 4.0, *)
@discardableResult
func CFURLEnumeratorGetDescendentLevel(_ enumerator: CFURLEnumerator!) -> CFIndex
