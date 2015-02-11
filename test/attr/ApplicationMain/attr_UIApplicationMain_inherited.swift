// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library -verify %s

// REQUIRES: objc_interop

import UIKit

class DelegateBase : NSObject, UIApplicationDelegate { }

@UIApplicationMain
class MyDelegate : DelegateBase { }

