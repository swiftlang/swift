// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache %s
// XFAIL: linux

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' class must conform to the 'NSApplicationDelegate' protocol}}
class MyNonDelegate {
}
