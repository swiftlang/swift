// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library -verify %s

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' class must conform to the 'NSApplicationDelegate' protocol}}
class MyNonDelegate {
}
