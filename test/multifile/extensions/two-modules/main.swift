// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/onone %t/wmo
// RUN: %target-build-swift -emit-module -emit-module-path %t/onone/library.swiftmodule -module-name=library -emit-library %S/Inputs/library.swift -o %t/onone/%target-library-name(rary)
// RUN: %target-build-swift %S/main.swift -I %t/onone/ -o %t/onone/main -L%t/onone -lrary

// RUN: %target-build-swift -emit-module -emit-module-path %t/wmo/library.swiftmodule -module-name=library -emit-library -O -wmo %S/Inputs/library.swift -o %t/wmo/%target-library-name(rary)
// RUN: %target-build-swift %S/main.swift -I %t/wmo/ -o %t/wmo/main -L%t/wmo -lrary

import library

extension Point {
  init(x: Int, y: Int) {
    self.x = x
    self.y = y

    // FIXME: Can't see the default initializer from another module?
    self.z = 0
  }
}
