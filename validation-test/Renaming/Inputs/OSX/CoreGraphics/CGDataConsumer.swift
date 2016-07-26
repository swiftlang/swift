
class CGDataConsumer {
}
typealias CGDataConsumerPutBytesCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafePointer<Void>, Int) -> Int
typealias CGDataConsumerReleaseInfoCallback = @convention(c) (UnsafeMutablePointer<Void>?) -> Void
struct CGDataConsumerCallbacks {
  var putBytes: CGDataConsumerPutBytesCallback?
  var releaseConsumer: CGDataConsumerReleaseInfoCallback?
  init()
  init(putBytes putBytes: CGDataConsumerPutBytesCallback?, releaseConsumer releaseConsumer: CGDataConsumerReleaseInfoCallback?)
}
extension CGDataConsumer {
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.0, *)
  init?(info info: UnsafeMutablePointer<Void>?, cbks cbks: UnsafePointer<CGDataConsumerCallbacks>?)
  @available(OSX 10.0, *)
  init?(with url: CFURL?)
  @available(OSX 10.4, *)
  init?(withCFData data: CFMutableData?)
}
