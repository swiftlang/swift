
class DRBurnSetupPanel : DRSetupPanel {
  func setDefaultButtonTitle(_ title: String!)
  func setCanSelectTestBurn(_ flag: Bool)
  func setCanSelectAppendableMedia(_ flag: Bool)
  @discardableResult
  func burnObject() -> DRBurn!
  @IBAction func expand(_ sender: AnyObject!)
  @IBAction func burnSpeed(_ sender: AnyObject!)
  @IBAction func appendable(_ sender: AnyObject!)
  @IBAction func completionAction(_ sender: AnyObject!)
  @IBAction func testBurn(_ sender: AnyObject!)
  @IBAction func verifyBurn(_ sender: AnyObject!)
}
@available(OSX 10.2, *)
let DRBurnSetupPanelDefaultButtonDefaultTitle: String
