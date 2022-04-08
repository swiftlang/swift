// RUN: %target-swift-ide-test -print-module -module-to-print=DictionaryOfNSStrings -I %S/Inputs/ -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: func takesDictionaryOfStrings(_ a: [String : String])