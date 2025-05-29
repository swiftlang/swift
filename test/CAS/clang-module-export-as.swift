// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name A_Internal -o %t/A_Internal.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -emit-module-interface-path %t/A_Internal.swiftinterface -enable-library-evolution -I %t %t/A_Internal.swift

// RUN: %target-swift-frontend -emit-module -module-name A -o %t/A.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -emit-module-interface-path %t/A.swiftinterface -enable-library-evolution -I %t %t/A.swift

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache %t/main.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas -swift-version 5

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:AC > %t/clangAC.cmd
// RUN: %swift_frontend_plain @%t/clangAC.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/clangA.cmd
// RUN: %swift_frontend_plain @%t/clangA.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule  \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-swift-modules -swift-version 5 -enable-cross-import-overlays \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd

//--- main.swift
import A

extension Base {
  func test(input: String) {
    Base.testA(1)
  }
}

//--- A.swift
@_exported import A
@_implementationOnly import A_Internal
public extension Base {
  static func testA(_ num: Int) {}
  fileprivate func testA(_ p: AI) {}
}

//--- A_Internal.swift
public struct AI {}

//--- a.h
#include "ac.h"

//--- ac.h
@interface Base
- (void)test;
@end

//--- module.modulemap
module A {
  header "a.h"
  export *
}

module AC {
  header "ac.h"
  export_as A
  export *
}
