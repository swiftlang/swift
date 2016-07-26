
@available(OSX 10.6, *)
let kICUTTypeRaw: CFString!
struct ICARawFileHeader {
  var imageDataOffset: UInt32
  var version: UInt32
  var imageWidth: UInt32
  var imageHeight: UInt32
  var bytesPerRow: UInt32
  var numberOfComponents: UInt32
  var bitsPerComponent: UInt32
  var bitsPerPixel: UInt32
  var cgColorSpaceModel: UInt32
  var bitmapInfo: UInt32
  var orientation: UInt32
  var dpi: UInt32
  var colorSyncModeStr: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
  init()
  init(imageDataOffset imageDataOffset: UInt32, version version: UInt32, imageWidth imageWidth: UInt32, imageHeight imageHeight: UInt32, bytesPerRow bytesPerRow: UInt32, numberOfComponents numberOfComponents: UInt32, bitsPerComponent bitsPerComponent: UInt32, bitsPerPixel bitsPerPixel: UInt32, cgColorSpaceModel cgColorSpaceModel: UInt32, bitmapInfo bitmapInfo: UInt32, orientation orientation: UInt32, dpi dpi: UInt32, colorSyncModeStr colorSyncModeStr: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8))
}
@discardableResult
func ICDCreateColorSpace(_ bitsPerPixel: UInt32, _ samplesPerPixel: UInt32, _ icaObject: ICAObject, _ colorSyncMode: CFString!, _ abstractProfile: CFData!, _ tmpProfilePath: UnsafeMutablePointer<Int8>!) -> Unmanaged<CGColorSpace>!
@discardableResult
func ICDAddImageInfoToNotificationDictionary(_ dict: CFMutableDictionary!, _ width: UInt32, _ height: UInt32, _ bytesPerRow: UInt32, _ dataStartRow: UInt32, _ dataNumberOfRows: UInt32, _ dataSize: UInt32, _ dataBuffer: UnsafeMutablePointer<Void>!) -> ICAError
@discardableResult
func ICDAddBandInfoToNotificationDictionary(_ dict: CFMutableDictionary!, _ width: UInt32, _ height: UInt32, _ bitsPerPixel: UInt32, _ bitsPerComponent: UInt32, _ numComponents: UInt32, _ endianness: UInt32, _ pixelDataType: UInt32, _ bytesPerRow: UInt32, _ dataStartRow: UInt32, _ dataNumberOfRows: UInt32, _ dataSize: UInt32, _ dataBuffer: UnsafeMutablePointer<Void>!) -> ICAError
@discardableResult
func ICDSendNotification(_ pb: UnsafeMutablePointer<ICASendNotificationPB>!) -> ICAError
@discardableResult
func ICDSendNotificationAndWaitForReply(_ pb: UnsafeMutablePointer<ICASendNotificationPB>!) -> ICAError
var ICLoggingEnabled: Int32
var ICLoggingLevelMask: Int32
let ICLoggingLevelError: Int32
let ICLoggingLevelWarning: Int32
let ICLoggingLevelBasicInfo: Int32
let ICLoggingLevelVerboseInfo: Int32
let ICLoggingLevelTimingInfo: Int32
