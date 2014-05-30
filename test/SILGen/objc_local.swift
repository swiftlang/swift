// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

func foo() {
  // CHECK-LABEL: sil [transparent] @_TToFCF10objc_local3fooFT_T_L_3Foog1xSi
  // CHECK-LABEL: sil [transparent] @_TToFCF10objc_local3fooFT_T_L_3Foos1xSi
  @objc class Foo { var x: Int = 0 }
}
