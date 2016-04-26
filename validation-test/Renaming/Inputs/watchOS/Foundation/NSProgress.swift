
@available(watchOS 2.0, *)
class NSProgress : NSObject {
  @discardableResult
  class func current() -> NSProgress?
  /*not inherited*/ init(totalUnitCount unitCount: Int64)
  @available(watchOS 2.0, *)
  @discardableResult
  class func discreteProgress(totalUnitCount unitCount: Int64) -> NSProgress
  @available(watchOS 2.0, *)
  /*not inherited*/ init(totalUnitCount unitCount: Int64, parent parent: NSProgress, pendingUnitCount portionOfParentTotalUnitCount: Int64)
  init(parent parentProgressOrNil: NSProgress?, userInfo userInfoOrNil: [NSObject : AnyObject]? = [:])
  func becomeCurrent(withPendingUnitCount unitCount: Int64)
  func resignCurrent()
  @available(watchOS 2.0, *)
  func addChild(_ child: NSProgress, withPendingUnitCount inUnitCount: Int64)
  var totalUnitCount: Int64
  var completedUnitCount: Int64
  var localizedDescription: String!
  var localizedAdditionalDescription: String!
  var isCancellable: Bool
  var isPausable: Bool
  var isCancelled: Bool { get }
  var isPaused: Bool { get }
  var cancellationHandler: (() -> Void)?
  var pausingHandler: (() -> Void)?
  @available(watchOS 2.0, *)
  var resumingHandler: (() -> Void)?
  func setUserInfoObject(_ objectOrNil: AnyObject?, forKey key: String)
  var isIndeterminate: Bool { get }
  var fractionCompleted: Double { get }
  func cancel()
  func pause()
  @available(watchOS 2.0, *)
  func resume()
  var userInfo: [NSObject : AnyObject] { get }
  var kind: String?
}
typealias NSProgressUnpublishingHandler = () -> Void
typealias NSProgressPublishingHandler = (NSProgress) -> NSProgressUnpublishingHandler?
protocol NSProgressReporting : NSObjectProtocol {
  @available(watchOS 2.0, *)
  var progress: NSProgress { get }
}
@available(watchOS 2.0, *)
let NSProgressEstimatedTimeRemainingKey: String
@available(watchOS 2.0, *)
let NSProgressThroughputKey: String
@available(watchOS 2.0, *)
let NSProgressKindFile: String
@available(watchOS 2.0, *)
let NSProgressFileOperationKindKey: String
@available(watchOS 2.0, *)
let NSProgressFileOperationKindDownloading: String
@available(watchOS 2.0, *)
let NSProgressFileOperationKindDecompressingAfterDownloading: String
@available(watchOS 2.0, *)
let NSProgressFileOperationKindReceiving: String
@available(watchOS 2.0, *)
let NSProgressFileOperationKindCopying: String
@available(watchOS 2.0, *)
let NSProgressFileURLKey: String
@available(watchOS 2.0, *)
let NSProgressFileTotalCountKey: String
@available(watchOS 2.0, *)
let NSProgressFileCompletedCountKey: String
