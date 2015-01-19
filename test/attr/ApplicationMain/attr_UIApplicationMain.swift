// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library -verify %s

import UIKit

@UIApplicationMain
class MyDelegate: NSObject, UIApplicationDelegate {
}
