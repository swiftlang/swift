// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache %s
// XFAIL: linux

import UIKit

class DelegateBase : NSObject, UIApplicationDelegate { }

@UIApplicationMain
class MyDelegate : DelegateBase { }

