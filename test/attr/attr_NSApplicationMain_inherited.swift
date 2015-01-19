// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library -verify %s

import AppKit

class DelegateBase : NSObject, NSApplicationDelegate { }

@NSApplicationMain
class MyDelegate : DelegateBase { }

