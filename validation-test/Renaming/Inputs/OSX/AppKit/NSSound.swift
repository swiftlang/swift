
let NSSoundPboardType: String
class NSSound : NSObject, NSCopying, NSCoding, NSPasteboardReading, NSPasteboardWriting {
  /*not inherited*/ init?(named name: String)
  init?(contentsOf url: NSURL, byReference byRef: Bool)
  init?(contentsOfFile path: String, byReference byRef: Bool)
  init?(data data: NSData)
  @discardableResult
  func setName(_ string: String?) -> Bool
  var name: String? { get }
  @discardableResult
  class func canInit(with pasteboard: NSPasteboard) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  class func soundUnfilteredTypes() -> [String]
  init?(pasteboard pasteboard: NSPasteboard)
  func write(to pasteboard: NSPasteboard)
  @discardableResult
  func play() -> Bool
  @discardableResult
  func pause() -> Bool
  @discardableResult
  func resume() -> Bool
  @discardableResult
  func stop() -> Bool
  var isPlaying: Bool { get }
  unowned(unsafe) var delegate: @sil_unmanaged NSSoundDelegate?
  @available(OSX 10.5, *)
  var duration: NSTimeInterval { get }
  @available(OSX 10.5, *)
  var volume: Float
  @available(OSX 10.5, *)
  var currentTime: NSTimeInterval
  @available(OSX 10.5, *)
  var loops: Bool
  @available(OSX 10.5, *)
  var playbackDeviceIdentifier: String?
}
extension NSSound {
}
protocol NSSoundDelegate : NSObjectProtocol {
  optional func sound(_ sound: NSSound, didFinishPlaying aBool: Bool)
}
extension NSBundle {
  @discardableResult
  func path(forSoundResource name: String) -> String?
}
