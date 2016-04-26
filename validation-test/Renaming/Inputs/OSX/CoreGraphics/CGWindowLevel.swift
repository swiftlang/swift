
enum CGWindowLevelKey : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case baseWindow
  case minimumWindow
  case desktopWindow
  case backstopMenu
  case normalWindow
  case floatingWindow
  case tornOffMenuWindow
  case dockWindow
  case mainMenuWindow
  case statusWindow
  case modalPanelWindow
  case popUpMenuWindow
  case draggingWindow
  case screenSaverWindow
  case maximumWindow
  case overlayWindow
  case helpWindow
  case utilityWindow
  case desktopIconWindow
  case cursorWindow
  case assistiveTechHighWindow
  case numberOfWindowLevelKeys
}
typealias CGWindowLevel = Int32
@available(OSX 10.0, *)
@discardableResult
func CGWindowLevelForKey(_ key: CGWindowLevelKey) -> CGWindowLevel
var kCGNumReservedWindowLevels: Int32 { get }
