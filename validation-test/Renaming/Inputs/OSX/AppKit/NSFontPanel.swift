
extension NSObject {
  @discardableResult
  class func validModes(for fontPanel: NSFontPanel) -> Int
  @discardableResult
  func validModes(for fontPanel: NSFontPanel) -> Int
}
class NSFontPanel : NSPanel {
  @discardableResult
  class func shared() -> NSFontPanel
  @discardableResult
  class func sharedFontPanelExists() -> Bool
  var accessoryView: NSView?
  func setPanelFont(_ fontObj: NSFont, isMultiple flag: Bool)
  @discardableResult
  func panelConvert(_ fontObj: NSFont) -> NSFont
  var isEnabled: Bool
  func reloadDefaultFontFamilies()
}
struct __fpFlags {
  var setFontChange: UInt32
  var setFontAttributeChange: UInt32
  var _delRespFamily: UInt32
  var _delRespFace: UInt32
  var _delRespSize: UInt32
  var _delRespColl: UInt32
  var _collectionDisabled: UInt32
  var _sizeDisabled: UInt32
  var _faceDisabled: UInt32
  var showEffects: UInt32
  var _uiMode: UInt32
  var _miniMode: UInt32
  var _reserved: UInt32
  init()
  init(setFontChange setFontChange: UInt32, setFontAttributeChange setFontAttributeChange: UInt32, _delRespFamily _delRespFamily: UInt32, _delRespFace _delRespFace: UInt32, _delRespSize _delRespSize: UInt32, _delRespColl _delRespColl: UInt32, _collectionDisabled _collectionDisabled: UInt32, _sizeDisabled _sizeDisabled: UInt32, _faceDisabled _faceDisabled: UInt32, showEffects showEffects: UInt32, _uiMode _uiMode: UInt32, _miniMode _miniMode: UInt32, _reserved _reserved: UInt32)
}
var NSFontPanelFaceModeMask: UInt32 { get }
var NSFontPanelSizeModeMask: UInt32 { get }
var NSFontPanelCollectionModeMask: UInt32 { get }
var NSFontPanelUnderlineEffectModeMask: UInt32 { get }
var NSFontPanelStrikethroughEffectModeMask: UInt32 { get }
var NSFontPanelTextColorEffectModeMask: UInt32 { get }
var NSFontPanelDocumentColorEffectModeMask: UInt32 { get }
var NSFontPanelShadowEffectModeMask: UInt32 { get }
var NSFontPanelAllEffectsModeMask: UInt32 { get }
var NSFontPanelStandardModesMask: UInt32 { get }
var NSFontPanelAllModesMask: UInt32 { get }
var NSFPPreviewButton: Int { get }
var NSFPRevertButton: Int { get }
var NSFPSetButton: Int { get }
var NSFPPreviewField: Int { get }
var NSFPSizeField: Int { get }
var NSFPSizeTitle: Int { get }
var NSFPCurrentField: Int { get }
