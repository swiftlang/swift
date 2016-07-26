
@available(tvOS, introduced: 5.0, deprecated: 9.0, message: "Access destinationViewController.popoverPresentationController from your segue's performHandler or override of -perform")
class UIStoryboardPopoverSegue : UIStoryboardSegue {
  var popoverController: UIPopoverController { get }
}
