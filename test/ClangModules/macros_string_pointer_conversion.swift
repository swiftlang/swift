// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -enable-string-pointer-conversion -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 %s
// RUN: ls -lR %t/clang-module-cache | FileCheck %s
// CHECK: macros{{.*}}.pcm

@exported import macros

func testCStrings() -> Bool {
  var str: ConstUnsafePointer<CChar> = UTF8_STRING
  str = VERSION_STRING
  return VERSION_STRING != nil && UTF8_STRING != nil
}
