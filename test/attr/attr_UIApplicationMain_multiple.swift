// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library -verify %s

import UIKit

@UIApplicationMain // expected-error{{'UIApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate1: NSObject, UIApplicationDelegate {
}

@UIApplicationMain // expected-error{{'UIApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate2: NSObject, UIApplicationDelegate {
}

@UIApplicationMain // expected-error{{'UIApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate3: NSObject, UIApplicationDelegate {
}
