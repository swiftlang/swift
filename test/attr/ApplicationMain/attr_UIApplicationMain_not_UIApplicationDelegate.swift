// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library -verify %s

import UIKit

@UIApplicationMain // expected-error{{'UIApplicationMain' class must conform to the 'UIApplicationDelegate' protocol}}
class MyNonDelegate {
}
