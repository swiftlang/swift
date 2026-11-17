// RUN: %empty-directory(%t)

// Swift 4 mode because initializing a cross-module struct's stored properties
// from an extension initializer is an error in Swift 5 and later.

// RUN: mkdir -p %t/onone %t/wmo
// RUN: %target-build-swift -emit-module -emit-module-path %t/onone/library.swiftmodule -module-name=library -emit-library %S/Inputs/library.swift -o %t/onone/%target-library-name(rary)
// RUN: %target-build-swift -language-mode 4 %S/main.swift -I %t/onone/ -o %t/onone/main -L%t/onone -lrary

// RUN: %target-build-swift -emit-module -emit-module-path %t/wmo/library.swiftmodule -module-name=library -emit-library -O -wmo %S/Inputs/library.swift -o %t/wmo/%target-library-name(rary)
// RUN: %target-build-swift -language-mode 4 %S/main.swift -I %t/wmo/ -o %t/wmo/main -L%t/wmo -lrary

import library

extension Point {
  init(x: Int, y: Int) {
    self.x = x
    self.y = y

    // FIXME: Can't see the default initializer from another module?
    self.z = 0
  }
}
