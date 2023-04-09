// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %s

// REQUIRES: objc_interop

import UIKit

@UIApplicationMain // expected-error{{'UIApplicationMain' class must conform to the 'UIApplicationDelegate' protocol}}
// expected-warning@-1 {{'UIApplicationMain' is deprecated; this is an error in Swift 6}}
// expected-note@-2 {{use @main instead}} {{1-19=@main}}
class MyNonDelegate {
}
