// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name A -o %t/A.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/A.swiftinterface -enable-library-evolution -I %t %t/A.swift

// RUN: %target-swift-frontend -emit-module -module-name B -o %t/B.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/B.swiftinterface -enable-library-evolution -I %t %t/B.swift

// RUN: %target-swift-frontend -emit-module -module-name _B_A -o %t/_B_A.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/_B_A.swiftinterface -enable-library-evolution -I %t %t/b_a.swift

// RUN: %target-swift-frontend -emit-module -module-name _C_A -o %t/_C_A.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-module-interface-path %t/_C_A.swiftinterface -enable-library-evolution -I %t %t/c_a.swift

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache %t/main.swift -F %t \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas -swift-version 5 -enable-cross-import-overlays -module-load-mode prefer-serialized

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:C > %t/C.cmd
// RUN: %swift_frontend_plain @%t/C.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json %t > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: %FileCheck %s --input-file=%t/MyApp.cmd --check-prefix CMD
// CMD: -swift-module-cross-import
// CMD-NEXT: [[CMI1:[B|C]]]
// CMD-NEXT: [[CMI1]].swiftcrossimport{{/|\\}}A.swiftoverlay
// CMD: -swift-module-cross-import
// CMD-NEXT: [[CMI2:[B|C]]]
// CMD-NEXT: [[CMI2]].swiftcrossimport{{/|\\}}A.swiftoverlay

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule  \
// RUN:   -emit-module-interface-path %t/Test.swiftinterface \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-swift-modules -swift-version 5 -enable-cross-import-overlays \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd

// RUN: %FileCheck %s --input-file=%t/Test.swiftinterface

/// Check to make sure the implicit cross import turned into explicit import in the interface file.
// CHECK: import _B_A
// CHECK: import _C_A

//--- A.swift
public func a() {}

//--- B.swift
public func b() {}

//--- b_a.swift
public func b_a() {}

//--- c_a.swift
public func c_a() {}

//--- C.framework/Modules/module.modulemap
framework module C {
  umbrella header "C.h"
  export *
}

//--- C.framework/Headers/C.h
void c(void);

//--- C.framework/Modules/C.swiftcrossimport/A.swiftoverlay
%YAML 1.2
---
version: 1
modules:
  - name: _C_A

//--- main.swift
import A
import B
import C

func test () {
  b_a()
  c_a()
}

//--- B.swiftcrossimport/A.swiftoverlay
%YAML 1.2
---
version: 1
modules:
  - name: _B_A
