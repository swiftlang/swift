// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -parse-as-library -nsstring-is-string -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s

import Foundation

func foo() {
  var f : (String) -> String = NSStringToNSString;
}
