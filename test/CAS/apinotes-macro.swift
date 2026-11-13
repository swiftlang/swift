// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -I %t/include

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -module-name Test \
// RUN:   -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift @%t/MyApp.cmd -swift-version 5 \
// RUN:   -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -typecheck

// RUN: rm -rf %t/include

// RUN: %target-swift-frontend-plain -module-name Test \
// RUN:   -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift @%t/MyApp.cmd -swift-version 5 \
// RUN:   -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -c -o %t/main.o -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS

// CACHE-MISS: remark: cache miss for input file

//--- main.swift
import A

func flag() -> Flags {
  // Without the API notes the macro is imported as CInt.
  return FLAG
}

//--- include/module.modulemap
module A { header "a.h" export * }

//--- include/a.h
typedef unsigned int Flags;
#define FLAG 1

//--- include/A.apinotes
Name: A
Globals:
  - Name: FLAG
    Type: Flags
