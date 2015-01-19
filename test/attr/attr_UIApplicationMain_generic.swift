// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library -verify %s

import UIKit

@UIApplicationMain // expected-error{{generic 'UIApplicationMain' classes are not supported}}
class MyDelegate<T>: NSObject, UIApplicationDelegate {
}
