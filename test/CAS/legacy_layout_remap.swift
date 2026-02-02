// REQUIRES: objc_interop
// REQUIRES: platform=Darwin
// UNSUPPORTED: swift_only_stable_abi

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build legacy module.
// RUN: %target-swift-frontend -target %target-pre-stable-abi-triple -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %S/../Inputs/resilient_struct.swift

/// Scan with legacy layout.
// RUN: %target-swift-frontend -target %target-pre-stable-abi-triple -I %t -c -enable-library-evolution -read-legacy-type-info-path=%t/layout.yaml \
// RUN:   -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 4 -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %t /^tmp

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend -target %target-pre-stable-abi-triple -enable-library-evolution \
// RUN:   -cache-compile-job -cas-path %t/cas -O \
// RUN:   -swift-version 4 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   /^tmp/main.swift @%t/MyApp.cmd -c -o %t/main.o

/// Now do implicit search.
// RUN: mkdir -p %t/resource/%target-os
// RUN: cp %t/layout.yaml %t/resource/%target-os/layouts-%target-arch.yaml

// RUN: %target-swift-frontend -target %target-pre-stable-abi-triple -I %t -c -enable-library-evolution \
// RUN:   -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O -module-load-mode prefer-serialized \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps2.json -swift-version 4 -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %t /^tmp -resource-dir %t/resource -I %platform-dylib-dir

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps2.json clang:SwiftShims > %t/shim2.cmd
// RUN: %swift_frontend_plain @%t/shim2.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps2.json > %t/map2.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map2.json > %t/map2.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps2.json Test > %t/MyApp2.cmd

// RUN: %target-swift-frontend -target %target-pre-stable-abi-triple -enable-library-evolution \
// RUN:   -cache-compile-job -cas-path %t/cas -O \
// RUN:   -swift-version 4 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map2.casid \
// RUN:   /^tmp/main.swift @%t/MyApp2.cmd -c -o %t/main.o

//--- main.swift
import resilient_struct

public class ClassWithResilientRef {
  var first: ResilientRef? = nil
  var second: Int = 0
}

//--- layout.yaml
Name:            resilient_struct
Decls:
  - Name:            16resilient_struct12ResilientRefV
    Size:            8
    Alignment:       8
    ExtraInhabitants: 4096
