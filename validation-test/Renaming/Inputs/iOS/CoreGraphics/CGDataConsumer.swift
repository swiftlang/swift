
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
  @available(iOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(iOS 2.0, *)
  init?(info info: UnsafeMutablePointer<Void>?, cbks cbks: UnsafePointer<CGDataConsumerCallbacks>?)
  @available(iOS 2.0, *)
  init?(with url: CFURL?)
  @available(iOS 2.0, *)
  init?(withCFData data: CFMutableData?)
}
