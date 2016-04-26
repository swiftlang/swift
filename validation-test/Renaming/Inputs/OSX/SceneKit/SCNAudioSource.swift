
@available(OSX 10.11, *)
class SCNAudioSource : NSObject, NSCopying, NSSecureCoding {
  convenience init?(fileNamed name: String)
  init?(url url: NSURL)
  convenience init?(named fileName: String)
  var isPositional: Bool
  var volume: Float
  var rate: Float
  var reverbBlend: Float
  var loops: Bool
  var shouldStream: Bool
  func load()
}
@available(OSX 10.11, *)
class SCNAudioPlayer : NSObject {
  init(source source: SCNAudioSource)
  var willStartPlayback: (() -> Void)?
  var didFinishPlayback: (() -> Void)?
  var audioSource: SCNAudioSource? { get }
}
extension SCNNode {
  @available(OSX 10.11, *)
  func addAudioPlayer(_ player: SCNAudioPlayer)
  @available(OSX 10.11, *)
  func removeAllAudioPlayers()
  @available(OSX 10.11, *)
  func removeAudioPlayer(_ player: SCNAudioPlayer)
  @available(OSX 10.11, *)
  var audioPlayers: [SCNAudioPlayer] { get }
}
