
@available(iOS 2.0, *)
class MPVolumeView : UIView, NSCoding {
  @available(iOS 4.2, *)
  var showsVolumeSlider: Bool
  @available(iOS 4.2, *)
  var showsRouteButton: Bool
  @available(iOS 7.0, *)
  var areWirelessRoutesAvailable: Bool { get }
  @available(iOS 7.0, *)
  var isWirelessRouteActive: Bool { get }
  @available(iOS 6.0, *)
  func setMinimumVolumeSliderImage(_ image: UIImage?, for state: UIControlState)
  @available(iOS 6.0, *)
  func setMaximumVolumeSliderImage(_ image: UIImage?, for state: UIControlState)
  @available(iOS 6.0, *)
  func setVolumeThumbImage(_ image: UIImage?, for state: UIControlState)
  @available(iOS 6.0, *)
  @discardableResult
  func minimumVolumeSliderImage(for state: UIControlState) -> UIImage?
  @available(iOS 6.0, *)
  @discardableResult
  func maximumVolumeSliderImage(for state: UIControlState) -> UIImage?
  @available(iOS 6.0, *)
  @discardableResult
  func volumeThumbImage(for state: UIControlState) -> UIImage?
  @available(iOS 7.0, *)
  var volumeWarningSliderImage: UIImage?
  @available(iOS 6.0, *)
  @discardableResult
  func volumeSliderRect(forBounds bounds: CGRect) -> CGRect
  @available(iOS 6.0, *)
  @discardableResult
  func volumeThumbRect(forBounds bounds: CGRect, volumeSliderRect rect: CGRect, value value: Float) -> CGRect
  @available(iOS 6.0, *)
  func setRouteButtonImage(_ image: UIImage?, for state: UIControlState)
  @available(iOS 6.0, *)
  @discardableResult
  func routeButtonImage(for state: UIControlState) -> UIImage?
  @available(iOS 6.0, *)
  @discardableResult
  func routeButtonRect(forBounds bounds: CGRect) -> CGRect
}
@available(iOS 7.0, *)
let MPVolumeViewWirelessRoutesAvailableDidChangeNotification: String
@available(iOS 7.0, *)
let MPVolumeViewWirelessRouteActiveDidChangeNotification: String
