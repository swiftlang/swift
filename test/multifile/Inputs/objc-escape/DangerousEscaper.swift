import Foundation

@objc(DangerousEscaper)
public protocol DangerousEscaper {
  @objc
  func mightBeNaughty(_ mayActuallyEscape: () -> ())
}
