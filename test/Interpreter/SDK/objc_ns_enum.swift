// RUN: %empty-directory(%t/cache)
// RUN: %target-run-simple-swift(-module-cache-path %t/cache)
// REQUIRES: executable_test

// NSButtonType (from AppKit) and UIViewAnimationCurve (from UIKit) both have
// private enumerators, so we preserve their values through raw value
// conversion operations.
// (Really we do this for all NS_ENUMs, though we reserve the right to do
// strict checking if we get a guarantee that certain types don't have
// hidden or future enumeration values.)

#if os(macOS)
import AppKit

print(NSButton.ButtonType(rawValue: 20721)!.rawValue)
#endif

#if os(iOS)
import UIKit

print(UIViewAnimationCurve(rawValue: 20721)!.rawValue)
#endif

// CHECK: 20721

