// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/onone %t/wmo
// RUN: %target-build-swift -emit-module -emit-module-path %t/onone/library.swiftmodule -module-name=library -emit-library %S/Inputs/library1.swift %S/Inputs/library2.swift -o %t/onone/%target-library-name(rary) -swift-version 4
// RUN: %target-build-swift %S/main.swift -I %t/onone/ -o %t/onone/main -swift-version 4 -L%t/onone -lrary

// RUN: %target-build-swift -emit-module -emit-module-path %t/wmo/library.swiftmodule -module-name=library -emit-library -O -wmo %S/Inputs/library1.swift %S/Inputs/library2.swift -o %t/wmo/%target-library-name(rary) -swift-version 4
// RUN: %target-build-swift %S/main.swift -I %t/wmo/ -o %t/wmo/main -swift-version 4 -L%t/wmo -lrary

import library

hasDefaultArgument()
