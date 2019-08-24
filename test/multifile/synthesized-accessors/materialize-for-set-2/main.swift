// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/onone %t/wmo
// RUN: %target-build-swift -emit-module -emit-module-path %t/onone/library.swiftmodule -I %S/Inputs/ -module-name=library %S/Inputs/library1.swift %S/Inputs/library2.swift
// RUN: %target-build-swift %S/main.swift -I %S/Inputs/ -I %t/onone/ -emit-ir > /dev/null

// RUN: %target-build-swift -emit-module -emit-module-path %t/wmo/library.swiftmodule -I %S/Inputs/ -module-name=library -wmo %S/Inputs/library1.swift %S/Inputs/library2.swift
// RUN: %target-build-swift %S/main.swift -I %S/Inputs/ -I %t/wmo/ -emit-ir > /dev/null

// REQUIRES: objc_interop

import Foundation
import library

class CustomCounter : MyCounter {
  override var value: Int32 { didSet { } }
}
