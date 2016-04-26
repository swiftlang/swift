
@available(iOS 3.0, *)
class MPMediaPickerController : UIViewController {
  init(mediaTypes mediaTypes: MPMediaType)
  var mediaTypes: MPMediaType { get }
  weak var delegate: @sil_weak MPMediaPickerControllerDelegate?
  var allowsPickingMultipleItems: Bool
  @available(iOS 6.0, *)
  var showsCloudItems: Bool
  @available(iOS 9.2, *)
  var showsItemsWithProtectedAssets: Bool
  var prompt: String?
}
protocol MPMediaPickerControllerDelegate : NSObjectProtocol {
  @available(iOS 3.0, *)
  optional func mediaPicker(_ mediaPicker: MPMediaPickerController, didPickMediaItems mediaItemCollection: MPMediaItemCollection)
  @available(iOS 3.0, *)
  optional func mediaPickerDidCancel(_ mediaPicker: MPMediaPickerController)
}
