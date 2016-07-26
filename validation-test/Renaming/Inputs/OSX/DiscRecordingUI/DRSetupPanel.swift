
class DRSetupPanel : NSPanel {
  init!(nibName nibName: String!)
  @discardableResult
  func run() -> Int
  func beginSetupSheet(for owner: NSWindow!, modalDelegate modalDelegate: AnyObject!, didEnd didEndSelector: Selector!, contextInfo contextInfo: UnsafeMutablePointer<Void>!)
  @IBAction func ok(_ sender: AnyObject!)
  @IBAction func cancel(_ sender: AnyObject!)
  @IBAction func eject(_ sender: AnyObject!)
  @IBAction func open(_ sender: AnyObject!)
  @IBAction func close(_ sender: AnyObject!)
  func deviceSelectionChanged(_ device: DRDevice!)
  @discardableResult
  func mediaStateChanged(_ status: [NSObject : AnyObject]!) -> Bool
  func setupForDisplay()
}
@available(OSX 10.2, *)
let DRSetupPanelDeviceSelectionChangedNotification: String
@available(OSX 10.2, *)
let DRSetupPanelSelectedDeviceKey: String
extension NSObject {
  @discardableResult
  class func setupPanel(_ aPanel: DRSetupPanel!, deviceCouldBeTarget device: DRDevice!) -> Bool
  @discardableResult
  func setupPanel(_ aPanel: DRSetupPanel!, deviceCouldBeTarget device: DRDevice!) -> Bool
  @discardableResult
  class func setupPanel(_ aPanel: DRSetupPanel!, determineBestDeviceOfA deviceA: DRDevice!, orB device: DRDevice!) -> DRDevice!
  @discardableResult
  func setupPanel(_ aPanel: DRSetupPanel!, determineBestDeviceOfA deviceA: DRDevice!, orB device: DRDevice!) -> DRDevice!
  class func setupPanelDeviceSelectionChanged(_ aNotification: NSNotification!)
  func setupPanelDeviceSelectionChanged(_ aNotification: NSNotification!)
  @discardableResult
  class func setupPanelShouldHandleMediaReservations(_ aPanel: DRSetupPanel!) -> Bool
  @discardableResult
  func setupPanelShouldHandleMediaReservations(_ aPanel: DRSetupPanel!) -> Bool
  @discardableResult
  class func setupPanel(_ aPanel: DRSetupPanel!, deviceContainsSuitableMedia device: DRDevice!, promptString prompt: AutoreleasingUnsafeMutablePointer<NSString?>!) -> Bool
  @discardableResult
  func setupPanel(_ aPanel: DRSetupPanel!, deviceContainsSuitableMedia device: DRDevice!, promptString prompt: AutoreleasingUnsafeMutablePointer<NSString?>!) -> Bool
}
