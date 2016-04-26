
@available(tvOS 7.0, *)
class GCGamepadSnapshot : GCGamepad {
  @NSCopying var snapshotData: NSData
  init(snapshotData data: NSData)
  init(controller controller: GCController, snapshotData data: NSData)
}
struct GCGamepadSnapShotDataV100 {
  var version: UInt16
  var size: UInt16
  var dpadX: Float
  var dpadY: Float
  var buttonA: Float
  var buttonB: Float
  var buttonX: Float
  var buttonY: Float
  var leftShoulder: Float
  var rightShoulder: Float
  init()
  init(version version: UInt16, size size: UInt16, dpadX dpadX: Float, dpadY dpadY: Float, buttonA buttonA: Float, buttonB buttonB: Float, buttonX buttonX: Float, buttonY buttonY: Float, leftShoulder leftShoulder: Float, rightShoulder rightShoulder: Float)
}
@available(tvOS 7.0, *)
@discardableResult
func GCGamepadSnapShotDataV100FromNSData(_ snapshotData: UnsafeMutablePointer<GCGamepadSnapShotDataV100>?, _ data: NSData?) -> Bool
@available(tvOS 7.0, *)
@discardableResult
func NSDataFromGCGamepadSnapShotDataV100(_ snapshotData: UnsafeMutablePointer<GCGamepadSnapShotDataV100>?) -> NSData?
