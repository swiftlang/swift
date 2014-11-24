// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache %s
// XFAIL: linux

import AppKit

@NSApplicationMain // expected-error{{generic 'NSApplicationMain' classes are not supported}}
class MyDelegate<T>: NSObject, NSApplicationDelegate {
}
