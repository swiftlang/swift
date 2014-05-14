// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache %s

import UIKit

@UIApplicationMain // expected-error{{generic 'UIApplicationMain' classes are not supported}}
class MyDelegate<T>: NSObject, UIApplicationDelegate {
}
