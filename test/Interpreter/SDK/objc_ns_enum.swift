// RUN: %target-run-simple-swift

// NSButtonType (from AppKit) and UIViewAnimationCurve (from UIKit) both have
// private enumerators, so we preserve their values through raw value
// conversion operations.
// (Really we do this for all NS_ENUMs, though we reserve the right to do
// strict checking if we get a guarantee that certain types don't have
// hidden or future enumeration values.)

#if os(OSX)
import AppKit

println(NSButtonType(rawValue: 20721)!.rawValue)
#endif

#if os(iOS)
import UIKit

println(UIViewAnimationCurve(rawValue: 20721)!.rawValue)
#endif

// CHECK: 20721

