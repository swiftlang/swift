
class CGDataProvider {
}
typealias CGDataProviderGetBytesCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>, Int) -> Int
typealias CGDataProviderSkipForwardCallback = @convention(c) (UnsafeMutablePointer<Void>?, off_t) -> off_t
typealias CGDataProviderRewindCallback = @convention(c) (UnsafeMutablePointer<Void>?) -> Void
typealias CGDataProviderReleaseInfoCallback = @convention(c) (UnsafeMutablePointer<Void>?) -> Void
struct CGDataProviderSequentialCallbacks {
  var version: UInt32
  var getBytes: CGDataProviderGetBytesCallback?
  var skipForward: CGDataProviderSkipForwardCallback?
  var rewind: CGDataProviderRewindCallback?
  var releaseInfo: CGDataProviderReleaseInfoCallback?
  init()
  init(version version: UInt32, getBytes getBytes: CGDataProviderGetBytesCallback?, skipForward skipForward: CGDataProviderSkipForwardCallback?, rewind rewind: CGDataProviderRewindCallback?, releaseInfo releaseInfo: CGDataProviderReleaseInfoCallback?)
}
typealias CGDataProviderGetBytePointerCallback = @convention(c) (UnsafeMutablePointer<Void>?) -> UnsafePointer<Void>?
typealias CGDataProviderReleaseBytePointerCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafePointer<Void>) -> Void
typealias CGDataProviderGetBytesAtPositionCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>, off_t, Int) -> Int
struct CGDataProviderDirectCallbacks {
  var version: UInt32
  var getBytePointer: CGDataProviderGetBytePointerCallback?
  var releaseBytePointer: CGDataProviderReleaseBytePointerCallback?
  var getBytesAtPosition: CGDataProviderGetBytesAtPositionCallback?
  var releaseInfo: CGDataProviderReleaseInfoCallback?
  init()
  init(version version: UInt32, getBytePointer getBytePointer: CGDataProviderGetBytePointerCallback?, releaseBytePointer releaseBytePointer: CGDataProviderReleaseBytePointerCallback?, getBytesAtPosition getBytesAtPosition: CGDataProviderGetBytesAtPositionCallback?, releaseInfo releaseInfo: CGDataProviderReleaseInfoCallback?)
}
extension CGDataProvider {
  @available(iOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(iOS 2.0, *)
  init?(sequentialInfo info: UnsafeMutablePointer<Void>?, callbacks callbacks: UnsafePointer<CGDataProviderSequentialCallbacks>?)
  @available(iOS 2.0, *)
  init?(directInfo info: UnsafeMutablePointer<Void>?, size size: off_t, callbacks callbacks: UnsafePointer<CGDataProviderDirectCallbacks>?)
  @available(iOS 2.0, *)
  init?(withDataInfo info: UnsafeMutablePointer<Void>?, data data: UnsafePointer<Void>?, size size: Int, releaseData releaseData: CGDataProviderReleaseDataCallback?)
  @available(iOS 2.0, *)
  init?(with data: CFData?)
  @available(iOS 2.0, *)
  init?(with url: CFURL?)
  @available(iOS 2.0, *)
  init?(withFilename filename: UnsafePointer<Int8>?)
  @available(iOS 2.0, *)
  @discardableResult
  func copyData() -> CFData?
}
typealias CGDataProviderReleaseDataCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafePointer<Void>, Int) -> Void
