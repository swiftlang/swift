// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

class X {
  // CHECK: sil [transparent] @_TToFC13objc_IBOutlet1Xg1sGSQSS_ : $@cc(objc_method) @thin (X) -> @autoreleased UncheckedOptional<NSString>
  // CHECK: sil @_TFC13objc_IBOutlet1XcfMS0_FT_S0_ : $@cc(method) @thin (@owned X) -> @owned X
  // CHECK: string_literal utf16 "Hello"
  @IBOutlet var s = "Hello"
  @IBOutlet var s2: String = "Hello"
}
