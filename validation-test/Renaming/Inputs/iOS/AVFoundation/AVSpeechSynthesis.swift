
@available(iOS 7.0, *)
enum AVSpeechBoundary : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case immediate
  case word
}
@available(iOS 9.0, *)
enum AVSpeechSynthesisVoiceQuality : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case enhanced
}
@available(iOS 7.0, *)
let AVSpeechUtteranceMinimumSpeechRate: Float
@available(iOS 7.0, *)
let AVSpeechUtteranceMaximumSpeechRate: Float
@available(iOS 7.0, *)
let AVSpeechUtteranceDefaultSpeechRate: Float
@available(iOS 9.0, *)
let AVSpeechSynthesisVoiceIdentifierAlex: String
@available(iOS 7.0, *)
class AVSpeechSynthesisVoice : NSObject, NSSecureCoding {
  @discardableResult
  class func speechVoices() -> [AVSpeechSynthesisVoice]
  @discardableResult
  class func currentLanguageCode() -> String
  /*not inherited*/ init?(language languageCode: String?)
  @available(iOS 9.0, *)
  /*not inherited*/ init?(identifier identifier: String)
  var language: String { get }
  @available(iOS 9.0, *)
  var identifier: String { get }
  @available(iOS 9.0, *)
  var name: String { get }
  @available(iOS 9.0, *)
  var quality: AVSpeechSynthesisVoiceQuality { get }
}
@available(iOS 7.0, *)
class AVSpeechUtterance : NSObject, NSCopying, NSSecureCoding {
  init(string string: String)
  var voice: AVSpeechSynthesisVoice?
  var speechString: String { get }
  var rate: Float
  var pitchMultiplier: Float
  var volume: Float
  var preUtteranceDelay: NSTimeInterval
  var postUtteranceDelay: NSTimeInterval
}
@available(iOS 7.0, *)
class AVSpeechSynthesizer : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged AVSpeechSynthesizerDelegate?
  var isSpeaking: Bool { get }
  var isPaused: Bool { get }
  func speak(_ utterance: AVSpeechUtterance)
  @discardableResult
  func stopSpeaking(at boundary: AVSpeechBoundary) -> Bool
  @discardableResult
  func pauseSpeaking(at boundary: AVSpeechBoundary) -> Bool
  @discardableResult
  func continueSpeaking() -> Bool
}
protocol AVSpeechSynthesizerDelegate : NSObjectProtocol {
  @available(iOS 7.0, *)
  optional func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didStart utterance: AVSpeechUtterance)
  @available(iOS 7.0, *)
  optional func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didFinish utterance: AVSpeechUtterance)
  @available(iOS 7.0, *)
  optional func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didPause utterance: AVSpeechUtterance)
  @available(iOS 7.0, *)
  optional func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didContinue utterance: AVSpeechUtterance)
  @available(iOS 7.0, *)
  optional func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didCancel utterance: AVSpeechUtterance)
  @available(iOS 7.0, *)
  optional func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, willSpeakRangeOfSpeechString characterRange: NSRange, utterance utterance: AVSpeechUtterance)
}
