// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default %s

// REQUIRES: objc_interop
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx || OS=ios

#if canImport(AppKit)
import AppKit

var _: AttributeScopes.AppKitAttributes.UnderlineStyleAttribute! = nil

#elseif canImport(UIKit)
import UIKit

var _: AttributeScopes.UIKitAttributes.UnderlineStyleAttribute! = nil

#endif
