// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache %s
// XFAIL: linux

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate1: NSObject, NSApplicationDelegate {
}

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate2: NSObject, NSApplicationDelegate {
}

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate3: NSObject, NSApplicationDelegate {
}
