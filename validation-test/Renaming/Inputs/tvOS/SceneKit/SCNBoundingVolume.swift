
protocol SCNBoundingVolume : NSObjectProtocol {
  @discardableResult
  func getBoundingBoxMin(_ min: UnsafeMutablePointer<SCNVector3>?, max max: UnsafeMutablePointer<SCNVector3>?) -> Bool
  @discardableResult
  func getBoundingSphereCenter(_ center: UnsafeMutablePointer<SCNVector3>?, radius radius: UnsafeMutablePointer<CGFloat>?) -> Bool
  @available(tvOS 8.0, *)
  func setBoundingBoxMin(_ min: UnsafeMutablePointer<SCNVector3>?, max max: UnsafeMutablePointer<SCNVector3>?)
}
