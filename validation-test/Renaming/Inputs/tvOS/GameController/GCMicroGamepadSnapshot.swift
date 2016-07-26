
@available(tvOS 7.0, *)
class GCMicroGamepadSnapshot : GCMicroGamepad {
  @NSCopying var snapshotData: NSData
  init(snapshotData data: NSData)
  init(controller controller: GCController, snapshotData data: NSData)
}
struct GCMicroGamepadSnapShotDataV100 {
  var version: UInt16
  var size: UInt16
  var dpadX: Float
  var dpadY: Float
  var buttonA: Float
  var buttonX: Float
  init()
  init(version version: UInt16, size size: UInt16, dpadX dpadX: Float, dpadY dpadY: Float, buttonA buttonA: Float, buttonX buttonX: Float)
}
@available(tvOS 7.0, *)
@discardableResult
func GCMicroGamepadSnapShotDataV100FromNSData(_ snapshotData: UnsafeMutablePointer<GCMicroGamepadSnapShotDataV100>?, _ data: NSData?) -> Bool
@available(tvOS 7.0, *)
@discardableResult
func NSDataFromGCMicroGamepadSnapShotDataV100(_ snapshotData: UnsafeMutablePointer<GCMicroGamepadSnapShotDataV100>?) -> NSData?
