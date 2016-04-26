
typealias AVMIDIPlayerCompletionHandler = () -> Void
@available(iOS 8.0, *)
class AVMIDIPlayer : NSObject {
  init(contentsOf inURL: NSURL, soundBankURL bankURL: NSURL?) throws
  init(data data: NSData, soundBankURL bankURL: NSURL?) throws
  func prepareToPlay()
  func play(_ completionHandler: AVMIDIPlayerCompletionHandler? = nil)
  func stop()
  var duration: NSTimeInterval { get }
  var isPlaying: Bool { get }
  var rate: Float
  var currentPosition: NSTimeInterval
}
