// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: cd %t && ln -s private src

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/src/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/src/include

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/src/main.swift @%t/MyApp.cmd

/// Test the same build but remapping the path.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t/src /^tmp -scanner-prefix-map-paths %t/sdk /^sdk \
// RUN:   %t/src/main.swift -o %t/deps-1.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/src/include

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json clang:A > %t/A-1.cmd
// RUN: %swift_frontend_plain @%t/A-1.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json B > %t/B-1.cmd
// RUN: %swift_frontend_plain @%t/B-1.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps-1.json > %t/map-1.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map-1.json > %t/map-1.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json Test > %t/MyApp-1.cmd
// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map-1.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t/src -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp-1.cmd

//--- private/main.swift
import A
import B

//--- private/include/module.modulemap
module A {
  header "A.h"
  export *
}

//--- private/include/A.h
void notused(void);

//--- private/include/B.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name B -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func c() { }

