
protocol CLLocationManagerDelegate : NSObjectProtocol {
  @available(watchOS 2.0, *)
  optional func locationManager(_ manager: CLLocationManager, didUpdate locations: [CLLocation])
  @available(watchOS 2.0, *)
  optional func locationManager(_ manager: CLLocationManager, didFailWithError error: NSError)
  @available(watchOS 2.0, *)
  optional func locationManager(_ manager: CLLocationManager, didChange status: CLAuthorizationStatus)
}
