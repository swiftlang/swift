// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default %s
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default %s -swift-version 6

// REQUIRES: objc_interop
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx || OS=ios

#if canImport(AppKit)
import AppKit

var _: AttributeScopes.AppKitAttributes.UnderlineStyleAttribute! = nil

var _ = NSEvent.SpecialKey.upArrow.rawValue
var _ = NSEvent.SpecialKey.enter.rawValue
var _ = NSUpArrowFunctionKey
var _ = NSEnterCharacter

#elseif canImport(UIKit)
import UIKit

var _: AttributeScopes.UIKitAttributes.UnderlineStyleAttribute! = nil

#endif
