// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %s

// REQUIRES: objc_interop

import UIKit

class DelegateBase : NSObject, UIApplicationDelegate { }

@UIApplicationMain // expected-warning {{'UIApplicationMain' is deprecated; this is an error in Swift 6}}
// expected-note@-1 {{use @main instead}} {{1-19=@main}}
class MyDelegate : DelegateBase { }

