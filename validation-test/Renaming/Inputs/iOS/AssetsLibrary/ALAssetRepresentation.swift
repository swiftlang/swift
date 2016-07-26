
@available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use PHImageRequestOptions with the PHImageManager from the Photos framework instead")
class ALAssetRepresentation : NSObject {
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use requestImageDataForAsset:options:resultHandler: on PHImageManager for a PHAsset to request image data from the Photos framework and check the dataUTI passed to your result handler instead")
  @discardableResult
  func uti() -> String!
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use requestImageForAsset:targetSize:contentMode:options:resultHandler: on PHImageManager to request a targetSize of image for a PHAsset from the Photos framework instead")
  @discardableResult
  func dimensions() -> CGSize
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use requestImageDataForAsset:options:resultHandler: on PHImageManager to request image data for a PHAsset from the Photos framework instead")
  @discardableResult
  func size() -> Int64
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use requestImageDataForAsset:options:resultHandler: on PHImageManager to request image data for a PHAsset from the Photos framework instead")
  @discardableResult
  func getBytes(_ buffer: UnsafeMutablePointer<UInt8>!, fromOffset offset: Int64, length length: Int, error error: NSErrorPointer) -> Int
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use requestImageForAsset:targetSize:contentMode:options:resultHandler: on PHImageManager to request a targetSize PHImageManagerMaximumSize for a PHAsset from the Photos framework instead")
  @discardableResult
  func fullResolutionImage() -> Unmanaged<CGImage>!
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use requestImageForAsset:targetSize:contentMode:options:resultHandler: on PHImageManager to request a targetSize of image for a PHAsset from the Photos framework instead")
  @discardableResult
  func cgImage(options options: [NSObject : AnyObject]! = [:]) -> Unmanaged<CGImage>!
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use requestImageForAsset:targetSize:contentMode:options:resultHandler: on PHImageManager to request a targetSize of image for a PHAsset from the Photos framework instead")
  @discardableResult
  func fullScreenImage() -> Unmanaged<CGImage>!
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use PHImageRequestOptions with the PHImageManager instead")
  @discardableResult
  func url() -> NSURL!
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use CGImageSourceCopyPropertiesAtIndex() to retrieve metadata from an image returned by the PHImageManager from the Photos framework instead")
  @discardableResult
  func metadata() -> [NSObject : AnyObject]!
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use the orientation of the UIImage returned for a PHAsset from the PHImageManager from the Photos framework instead")
  @discardableResult
  func orientation() -> ALAssetOrientation
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Use requestImageForAsset:targetSize:contentMode:options:resultHandler: on PHImageManager to request a targetSize of image for a PHAsset from the Photos framework instead")
  @discardableResult
  func scale() -> Float
  @available(iOS, introduced: 5.0, deprecated: 9.0, message: "Use the Photos framework instead")
  @discardableResult
  func filename() -> String!
}
