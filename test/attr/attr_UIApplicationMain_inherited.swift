// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library -verify %s

import UIKit

class DelegateBase : NSObject, UIApplicationDelegate { }

@UIApplicationMain
class MyDelegate : DelegateBase { }

