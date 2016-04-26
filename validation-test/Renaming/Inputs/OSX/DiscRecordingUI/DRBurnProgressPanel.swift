
@available(OSX 10.2, *)
let DRBurnProgressPanelWillBeginNotification: String
@available(OSX 10.2, *)
let DRBurnProgressPanelDidFinishNotification: String
class DRBurnProgressPanel : NSPanel {
  func beginProgressSheet(for burn: DRBurn!, layout layout: AnyObject!, modalFor docWindow: NSWindow!)
  func begin(for burn: DRBurn!, layout layout: AnyObject!)
  func setDescription(_ description: String!)
  func setVerboseProgressStatus(_ verbose: Bool)
  @discardableResult
  func verboseProgressStatus() -> Bool
  @IBAction func stopBurn(_ sender: AnyObject!)
}
extension NSObject {
  class func burnProgressPanelWillBegin(_ aNotification: NSNotification!)
  func burnProgressPanelWillBegin(_ aNotification: NSNotification!)
  class func burnProgressPanelDidFinish(_ aNotification: NSNotification!)
  func burnProgressPanelDidFinish(_ aNotification: NSNotification!)
  @discardableResult
  class func burnProgressPanel(_ theBurnPanel: DRBurnProgressPanel!, burnDidFinish burn: DRBurn!) -> Bool
  @discardableResult
  func burnProgressPanel(_ theBurnPanel: DRBurnProgressPanel!, burnDidFinish burn: DRBurn!) -> Bool
}
