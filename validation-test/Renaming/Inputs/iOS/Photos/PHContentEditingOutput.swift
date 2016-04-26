
@available(iOS 8.0, *)
class PHContentEditingOutput : NSObject {
  init(contentEditingInput contentEditingInput: PHContentEditingInput)
  var adjustmentData: PHAdjustmentData?
  @NSCopying var renderedContentURL: NSURL { get }
}
