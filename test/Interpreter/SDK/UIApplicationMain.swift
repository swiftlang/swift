// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/main -parse-as-library

// REQUIRES: rdar106965845

// We aren't yet able to run tests that require a UI context, so just try
// building with the real SDK for now.
// DISABLED: %target-run %t/main | %FileCheck %s

// REQUIRES: OS=ios

import UIKit

@UIApplicationMain
class MyDelegate : NSObject, UIApplicationDelegate {
  func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject: AnyObject]?) -> Bool {
    // CHECK: launched
    print("launched")
    exit(EXIT_SUCCESS)
  }
}
