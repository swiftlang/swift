// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library -verify %s

import AppKit

@NSApplicationMain
class MyDelegate: NSObject, NSApplicationDelegate {
}
