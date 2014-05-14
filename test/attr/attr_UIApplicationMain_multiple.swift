// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache %s

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
