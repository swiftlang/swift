
class ICScannerBandData : NSObject {
  var fullImageWidth: Int { get }
  var fullImageHeight: Int { get }
  var bitsPerPixel: Int { get }
  var bitsPerComponent: Int { get }
  var numComponents: Int { get }
  var isBigEndian: Bool { get }
  var pixelDataType: ICScannerPixelDataType { get }
  var colorSyncProfilePath: String? { get }
  var bytesPerRow: Int { get }
  var dataStartRow: Int { get }
  var dataNumRows: Int { get }
  var dataSize: Int { get }
  var dataBuffer: NSData? { get }
}
