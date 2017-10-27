// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/onone %t/wmo
// RUN: %target-build-swift -emit-module -emit-module-path %t/onone/library.swiftmodule -module-name=library -emit-library %S/Inputs/library.swift -o %t/onone/library.%target-dylib-extension
// RUN: %target-build-swift %S/main.swift %t/onone/library.%target-dylib-extension -I %t/onone/ -o %t/onone/main

// RUN: %target-build-swift -emit-module -emit-module-path %t/wmo/library.swiftmodule -module-name=library -emit-library -O -wmo %S/Inputs/library.swift -o %t/wmo/library.%target-dylib-extension
// RUN: %target-build-swift %S/main.swift %t/wmo/library.%target-dylib-extension -I %t/wmo/ -o %t/wmo/main

import library

extension Point {
  init(x: Int, y: Int) {
    self.x = x
    self.y = y

    // FIXME: Can't see the default initializer from another module?
    self.z = 0
  }
}
