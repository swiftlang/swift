// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -application-extension %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -application-extension-library %s

// REQUIRES: OS=macosx

import AppKit
import Foundation
import AvailabilityExtras

func test_unavailable_because_deprecated() {
  _ = NSRealMemoryAvailable() // expected-error {{APIs deprecated as of macOS 10.9 and earlier are unavailable in Swift}}
}

func test_swift_unavailable_wins() {
  unavailableWithOS() // expected-error {{'unavailableWithOS()' is unavailable in Swift}}
}

func bezierPathElementToInteger(_ e: NSBezierPathElement) -> Int {
  switch e {
  case .moveTo: return 1
  case .lineTo: return 2
  case .curveTo: return 3 // no error
  case .cubicCurveTo: return 3
  case .closePath: return 4
  case .quadraticCurveTo: return 5
  @unknown default: return -1
  }
}

func integerToBezierPathElement(_ i: Int) -> NSBezierPathElement {
  // expected-note@-1 2 {{add '@available' attribute to enclosing global function}}
  switch i {
  case 1:
    return .moveTo
  case 2:
    return .lineTo
  case 3:
    return .curveTo // expected-error {{'curveTo' is only available in}}
    // expected-note@-1 {{add 'if #available' version check}}
  case 4:
    return .closePath
  case 5:
    return .quadraticCurveTo // expected-error {{'quadraticCurveTo' is only available in}}
    // expected-note@-1 {{add 'if #available' version check}}
  default:
    fatalError()
  }
}
