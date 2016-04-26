
class DRTrack : NSObject {
  init!(producer producer: AnyObject!)
  @discardableResult
  func properties() -> [NSObject : AnyObject]!
  func setProperties(_ properties: [NSObject : AnyObject]!)
  @discardableResult
  func testProductionSpeed(forInterval interval: NSTimeInterval) -> Float
  @discardableResult
  func testProductionSpeed(forLength length: UInt32) -> Float
  @discardableResult
  func estimateLength() -> UInt64
}
extension DRTrack {
  @discardableResult
  func length() -> DRMSF!
  @discardableResult
  func preGap() -> DRMSF!
  func setPreGap(_ preGap: DRMSF!)
}
protocol DRTrackDataProduction {
  @discardableResult
  func estimateLength(of track: DRTrack!) -> UInt64
  @available(OSX 10.0, *)
  @discardableResult
  func prepare(_ track: DRTrack!, for burn: DRBurn!, toMedia mediaInfo: [NSObject : AnyObject]!) -> Bool
  func cleanupTrack(afterBurn track: DRTrack!)
  @discardableResult
  func producePreGap(for track: DRTrack!, intoBuffer buffer: UnsafeMutablePointer<Int8>!, length bufferLength: UInt32, atAddress address: UInt64, blockSize blockSize: UInt32, ioFlags flags: UnsafeMutablePointer<UInt32>!) -> UInt32
  @discardableResult
  func produceData(for track: DRTrack!, intoBuffer buffer: UnsafeMutablePointer<Int8>!, length bufferLength: UInt32, atAddress address: UInt64, blockSize blockSize: UInt32, ioFlags flags: UnsafeMutablePointer<UInt32>!) -> UInt32
  @discardableResult
  func prepareTrack(forVerification track: DRTrack!) -> Bool
  @discardableResult
  func verifyPreGap(for track: DRTrack!, inBuffer buffer: UnsafePointer<Int8>!, length bufferLength: UInt32, atAddress address: UInt64, blockSize blockSize: UInt32, ioFlags flags: UnsafeMutablePointer<UInt32>!) -> Bool
  @discardableResult
  func verifyData(for track: DRTrack!, inBuffer buffer: UnsafePointer<Int8>!, length bufferLength: UInt32, atAddress address: UInt64, blockSize blockSize: UInt32, ioFlags flags: UnsafeMutablePointer<UInt32>!) -> Bool
  @discardableResult
  func cleanupTrack(afterVerification track: DRTrack!) -> Bool
}
@available(OSX 10.2, *)
let DRTrackLengthKey: String
@available(OSX 10.2, *)
let DRBlockSizeKey: String
@available(OSX 10.2, *)
let DRBlockTypeKey: String
@available(OSX 10.2, *)
let DRDataFormKey: String
@available(OSX 10.2, *)
let DRSessionFormatKey: String
@available(OSX 10.2, *)
let DRTrackModeKey: String
@available(OSX 10.2, *)
let DRVerificationTypeKey: String
@available(OSX 10.2, *)
let DRMaxBurnSpeedKey: String
@available(OSX 10.2, *)
let DRPreGapLengthKey: String
@available(OSX 10.5, *)
let DRPreGapIsRequiredKey: String
@available(OSX 10.2, *)
let DRDVDTimestampKey: String
@available(OSX 10.2, *)
let DRDVDCopyrightInfoKey: String
@available(OSX 10.3, *)
let DRTrackISRCKey: String
@available(OSX 10.3, *)
let DRIndexPointsKey: String
@available(OSX 10.3, *)
let DRAudioPreEmphasisKey: String
@available(OSX 10.3, *)
let DRAudioFourChannelKey: String
@available(OSX 10.3, *)
let DRSerialCopyManagementStateKey: String
@available(OSX 10.2, *)
let DRVerificationTypeProduceAgain: String
@available(OSX 10.2, *)
let DRVerificationTypeReceiveData: String
@available(OSX 10.4, *)
let DRVerificationTypeChecksum: String
@available(OSX 10.2, *)
let DRVerificationTypeNone: String
@available(OSX 10.3, *)
let DRSCMSCopyrightFree: String
@available(OSX 10.3, *)
let DRSCMSCopyrightProtectedOriginal: String
@available(OSX 10.3, *)
let DRSCMSCopyrightProtectedCopy: String
@available(OSX 10.3, *)
let DRNextWritableAddressKey: String
@available(OSX 10.3, *)
let DRTrackStartAddressKey: String
@available(OSX 10.3, *)
let DRFreeBlocksKey: String
@available(OSX 10.3, *)
let DRTrackNumberKey: String
@available(OSX 10.3, *)
let DRSessionNumberKey: String
@available(OSX 10.3, *)
let DRTrackTypeKey: String
@available(OSX 10.3, *)
let DRTrackIsEmptyKey: String
@available(OSX 10.3, *)
let DRTrackPacketTypeKey: String
@available(OSX 10.3, *)
let DRTrackPacketSizeKey: String
@available(OSX 10.3, *)
let DRTrackTypeInvisible: String
@available(OSX 10.3, *)
let DRTrackTypeIncomplete: String
@available(OSX 10.3, *)
let DRTrackTypeReserved: String
@available(OSX 10.3, *)
let DRTrackTypeClosed: String
@available(OSX 10.3, *)
let DRTrackPacketTypeFixed: String
@available(OSX 10.3, *)
let DRTrackPacketTypeVariable: String
@available(OSX 10.2, *)
let DRISOLevel: String
@available(OSX 10.2, *)
let DRVolumeSet: String
@available(OSX 10.2, *)
let DRPublisher: String
@available(OSX 10.2, *)
let DRDataPreparer: String
@available(OSX 10.2, *)
let DRApplicationIdentifier: String
@available(OSX 10.2, *)
let DRSystemIdentifier: String
@available(OSX 10.2, *)
let DRCopyrightFile: String
@available(OSX 10.2, *)
let DRAbstractFile: String
@available(OSX 10.2, *)
let DRBibliographicFile: String
@available(OSX 10.2, *)
let DRBlockSize: String
@available(OSX 10.2, *)
let DRDefaultDate: String
@available(OSX 10.2, *)
let DRVolumeCreationDate: String
@available(OSX 10.2, *)
let DRVolumeModificationDate: String
@available(OSX 10.2, *)
let DRVolumeCheckedDate: String
@available(OSX 10.2, *)
let DRVolumeExpirationDate: String
@available(OSX 10.2, *)
let DRVolumeEffectiveDate: String
@available(OSX 10.2, *)
let DRISOMacExtensions: String
@available(OSX 10.2, *)
let DRISORockRidgeExtensions: String
@available(OSX 10.3, *)
let DRSuppressMacSpecificFiles: String
var DRFlagSubchannelDataRequested: Int { get }
@available(OSX 10.5, *)
let DRSubchannelDataFormKey: String
@available(OSX 10.5, *)
let DRSubchannelDataFormNone: String
@available(OSX 10.5, *)
let DRSubchannelDataFormPack: String
@available(OSX 10.5, *)
let DRSubchannelDataFormRaw: String
