
enum AVAssetReaderStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case reading
  case completed
  case failed
  case cancelled
}
@available(tvOS 4.1, *)
class AVAssetReader : NSObject {
  init(asset asset: AVAsset) throws
  var asset: AVAsset { get }
  var status: AVAssetReaderStatus { get }
  var error: NSError? { get }
  var timeRange: CMTimeRange
  var outputs: [AVAssetReaderOutput] { get }
  @discardableResult
  func canAddOutput(_ output: AVAssetReaderOutput) -> Bool
  func addOutput(_ output: AVAssetReaderOutput)
  @discardableResult
  func startReading() -> Bool
  func cancelReading()
}
