
typealias AVAudioFramePosition = Int64
typealias AVAudioFrameCount = UInt32
typealias AVAudioPacketCount = UInt32
typealias AVAudioChannelCount = UInt32
typealias AVAudioNodeCompletionHandler = () -> Void
typealias AVAudioNodeBus = Int
struct AVAudio3DPoint {
  var x: Float
  var y: Float
  var z: Float
  init()
  init(x x: Float, y y: Float, z z: Float)
}
@discardableResult
func AVAudioMake3DPoint(_ x: Float, _ y: Float, _ z: Float) -> AVAudio3DPoint
typealias AVAudio3DVector = AVAudio3DPoint
@discardableResult
func AVAudioMake3DVector(_ x: Float, _ y: Float, _ z: Float) -> AVAudio3DVector
struct AVAudio3DVectorOrientation {
  var forward: AVAudio3DVector
  var up: AVAudio3DVector
  init()
  init(forward forward: AVAudio3DVector, up up: AVAudio3DVector)
}
@discardableResult
func AVAudioMake3DVectorOrientation(_ forward: AVAudio3DVector, _ up: AVAudio3DVector) -> AVAudio3DVectorOrientation
struct AVAudio3DAngularOrientation {
  var yaw: Float
  var pitch: Float
  var roll: Float
  init()
  init(yaw yaw: Float, pitch pitch: Float, roll roll: Float)
}
@discardableResult
func AVAudioMake3DAngularOrientation(_ yaw: Float, _ pitch: Float, _ roll: Float) -> AVAudio3DAngularOrientation
