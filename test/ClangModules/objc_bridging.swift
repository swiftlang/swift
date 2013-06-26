// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -parse-as-library -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s

import Foundation

extension String {
  func onlyOnString() -> String { return this }
}

extension Bool {
  func onlyOnBool() -> Bool { return this }
}

func foo() {
  var sf : (String) -> String = NSStringToNSString
  var s : String = NSArray().nsstringProperty.onlyOnString()

  var bf : (Bool) -> Bool = BOOLtoBOOL
  var b : Bool = NSArray().boolProperty.onlyOnBool()
}
