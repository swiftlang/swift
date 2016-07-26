
@available(tvOS 8.0, *)
class AVAudioChannelLayout : NSObject, NSSecureCoding {
  convenience init(layoutTag layoutTag: AudioChannelLayoutTag)
  init(layout layout: UnsafePointer<AudioChannelLayout>)
  var layoutTag: AudioChannelLayoutTag { get }
  var layout: UnsafePointer<AudioChannelLayout> { get }
  var channelCount: AVAudioChannelCount { get }
}
