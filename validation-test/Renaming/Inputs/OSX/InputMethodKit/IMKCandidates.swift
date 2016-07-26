
var kIMKSingleColumnScrollingCandidatePanel: Int { get }
var kIMKScrollingGridCandidatePanel: Int { get }
var kIMKSingleRowSteppingCandidatePanel: Int { get }
typealias IMKCandidatePanelType = Int
var kIMKMain: Int { get }
var kIMKAnnotation: Int { get }
var kIMKSubList: Int { get }
typealias IMKStyleType = Int
var kIMKLocateCandidatesAboveHint: Int { get }
var kIMKLocateCandidatesBelowHint: Int { get }
var kIMKLocateCandidatesLeftHint: Int { get }
var kIMKLocateCandidatesRightHint: Int { get }
typealias IMKCandidatesLocationHint = Int
let IMKCandidatesOpacityAttributeName: String
let IMKCandidatesSendServerKeyEventFirst: String
class IMKCandidates : NSResponder {
  init!(server server: IMKServer!, panelType panelType: IMKCandidatePanelType)
  init!(server server: IMKServer!, panelType panelType: IMKCandidatePanelType, styleType style: IMKStyleType)
  @discardableResult
  func panelType() -> IMKCandidatePanelType
  func setPanelType(_ panelType: IMKCandidatePanelType)
  func show(_ locationHint: IMKCandidatesLocationHint)
  func hide()
  @discardableResult
  func isVisible() -> Bool
  func update()
  func showAnnotation(_ annotationString: NSAttributedString!)
  func showSublist(_ candidates: [AnyObject]!, subListDelegate delegate: AnyObject!)
  @discardableResult
  func selectedCandidateString() -> NSAttributedString!
  func setCandidateFrameTopLeft(_ point: NSPoint)
  @discardableResult
  func candidateFrame() -> NSRect
  func setSelectionKeys(_ keyCodes: [AnyObject]!)
  @discardableResult
  func selectionKeys() -> [AnyObject]!
  func setSelectionKeysKeylayout(_ layout: TISInputSource!)
  @discardableResult
  func selectionKeysKeylayout() -> Unmanaged<TISInputSource>!
  func setAttributes(_ attributes: [NSObject : AnyObject]!)
  @discardableResult
  func attributes() -> [NSObject : AnyObject]!
  func setDismissesAutomatically(_ flag: Bool)
  @discardableResult
  func dismissesAutomatically() -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  func selectedCandidate() -> Int
  @available(OSX 10.7, *)
  func showChild()
  @available(OSX 10.7, *)
  func hideChild()
  @available(OSX 10.7, *)
  func attachChild(_ child: IMKCandidates!, toCandidate candidateIdentifier: Int, type theType: IMKStyleType)
  @available(OSX 10.7, *)
  func detachChild(_ candidateIdentifier: Int)
  @available(OSX 10.7, *)
  func setCandidateData(_ candidatesArray: [AnyObject]!)
  @available(OSX 10.7, *)
  @discardableResult
  func selectCandidate(withIdentifier candidateIdentifier: Int) -> Bool
  func selectCandidate(_ candidateIdentifier: Int)
  @available(OSX 10.7, *)
  func show()
  @available(OSX 10.7, *)
  @discardableResult
  func candidateStringIdentifier(_ candidateString: AnyObject!) -> Int
  @available(OSX 10.7, *)
  @discardableResult
  func candidateIdentifier(atLineNumber lineNumber: Int) -> Int
  @available(OSX 10.7, *)
  @discardableResult
  func lineNumberForCandidate(withIdentifier candidateIdentifier: Int) -> Int
  @available(OSX 10.7, *)
  func clearSelection()
}
