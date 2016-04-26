
@available(iOS 9.0, *)
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
@available(iOS 9.0, *)
class SCNAudioPlayer : NSObject {
  init(source source: SCNAudioSource)
  var willStartPlayback: (() -> Void)?
  var didFinishPlayback: (() -> Void)?
  var audioSource: SCNAudioSource? { get }
}
extension SCNNode {
  @available(iOS 9.0, *)
  func addAudioPlayer(_ player: SCNAudioPlayer)
  @available(iOS 9.0, *)
  func removeAllAudioPlayers()
  @available(iOS 9.0, *)
  func removeAudioPlayer(_ player: SCNAudioPlayer)
  @available(iOS 9.0, *)
  var audioPlayers: [SCNAudioPlayer] { get }
}
