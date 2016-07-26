
typealias MKMapSnapshotCompletionHandler = (MKMapSnapshot?, NSError?) -> Void
@available(tvOS 9.2, *)
class MKMapSnapshotter : NSObject {
  init(options options: MKMapSnapshotOptions)
  func start(completionHandler completionHandler: MKMapSnapshotCompletionHandler)
  func start(with queue: dispatch_queue_t, completionHandler completionHandler: MKMapSnapshotCompletionHandler)
  func cancel()
  var isLoading: Bool { get }
}
