// REQUIRES: objc_interop
// RUN: rm -rf %t.mod && mkdir -p %t.mod
// RUN: %target-swift-frontend -emit-module -o %t.mod/MyAppKit.swiftmodule %S/Inputs/MyAppKit.swift -module-name MyAppKit -parse-as-library
// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -update-code -primary-file %s -I %t.mod -emit-migrated-file-path %t/api-special-cases.swift.result -emit-remap-file-path %t/api-special-cases.swift.remap -o /dev/null -api-diff-data-file %S/SpecialCaseAPI.json
// RUN: diff -u %S/api-special-cases.swift.expected %t/api-special-cases.swift.result

import MyAppKit

func foo(_ Opt: NSOpenGLOption) {
  var Value = 1
  NSOpenGLSetOption(Opt, 1)
  NSOpenGLGetOption(Opt, &Value)
}
