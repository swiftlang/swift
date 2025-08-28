// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include -sdk %t/sdk \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp -scanner-prefix-map-paths %t/sdk /^sdk -enable-cross-import-overlays

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json _B_A > %t/BA.cmd
// RUN: %swift_frontend_plain @%t/BA.cmd

// RUN: %FileCheck %s --check-prefix=SDK-REMAP --input-file=%t/A.cmd
// RUN: %FileCheck %s --check-prefix=SDK-REMAP --input-file=%t/B.cmd
// RUN: %FileCheck %s --check-prefix=SDK-REMAP --input-file=%t/BA.cmd

// SDK-REMAP: -isysroot
// SDK-REMAP-NEXT: -Xcc
// SDK-REMAP-NEXT: /^sdk

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %FileCheck %s --check-prefix=SDK-REMAP --input-file=%t/MyApp.cmd

// RUN: %target-swift-frontend \
// RUN:   -c -o %t/main.o -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp.cmd -enable-cross-import-overlays

// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 0 -- \
// RUN:   %target-swift-frontend \
// RUN:   -c -o %t/main.o -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp.cmd -enable-cross-import-overlays > %t/key.casid

// RUN: %swift-scan-test -action replay_result -cas-path %t/cas -id @%t/key.casid -- \
// RUN:   %target-swift-frontend \
// RUN:   -c -o %t/main.o -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp.cmd -enable-cross-import-overlays

//--- main.swift
import A
import B

#warning("This is a warning")

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func a() { }

//--- include/B.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name B -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func b() { }

//--- include/_B_A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name _B_A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func b_a() { }

//--- include/B.swiftcrossimport/A.swiftoverlay
%YAML 1.2
---
version: 1
modules:
  - name: _B_A
//--- sdk/SDKSettings.json
{}
