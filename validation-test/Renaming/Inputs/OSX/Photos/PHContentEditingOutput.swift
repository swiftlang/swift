
@available(OSX 10.11, *)
class PHContentEditingOutput : NSObject {
  init(contentEditingInput contentEditingInput: PHContentEditingInput)
  var adjustmentData: PHAdjustmentData
  @NSCopying var renderedContentURL: NSURL { get }
}
