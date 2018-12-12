// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/onone %t/wmo
// RUN: %target-build-swift -emit-module -emit-module-path %t/onone/library.swiftmodule -module-name=library -emit-library %S/Inputs/library1.swift %S/Inputs/library2.swift -o %t/onone/library%{target-shared-library-suffix} -swift-version 4
// RUN: %target-build-swift %S/main.swift %t/onone/library%{target-shared-library-suffix} -I %t/onone/ -o %t/onone/main -swift-version 4

// RUN: %target-build-swift -emit-module -emit-module-path %t/wmo/library.swiftmodule -module-name=library -emit-library -O -wmo %S/Inputs/library1.swift %S/Inputs/library2.swift -o %t/wmo/library%{target-shared-library-suffix} -swift-version 4
// RUN: %target-build-swift %S/main.swift %t/wmo/library%{target-shared-library-suffix} -I %t/wmo/ -o %t/wmo/main -swift-version 4

import library

hasDefaultArgument()
