// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library -verify %s

// REQUIRES: objc_interop

import AppKit

class DelegateBase : NSObject, NSApplicationDelegate { }

@NSApplicationMain
class MyDelegate : DelegateBase { }

