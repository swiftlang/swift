// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache %t/main.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -o %t/deps.json -I %t/include -swift-version 4 -cache-compile-job -cas-path %t/cas 
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:DotDot > %t/DotDot.cmd
// RUN: %swift_frontend_plain @%t/DotDot.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd

/// Test that if there are non-existing module-map file passed through -Xcc, this still compiles.
// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd -Xcc -fmodule-map-file=%t/do-not-exist.modulemap

//--- main.swift
import DotDot

//--- include/module.modulemap
module DotDot [extern_c] {
  header "../dotdot.h"
  export *
}

//--- dotdot.h
void test(void);
