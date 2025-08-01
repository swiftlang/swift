// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include -sdk %t/sdk

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd
// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend \
// RUN:   -c -o %t/main.o -cache-compile-job -cas-path %t/cas -Rcache-compile-job \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd -emit-dependencies -emit-dependencies-path %t/main.d 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS

// CACHE-MISS: remark: cache miss for input

// RUN: %FileCheck %s --check-prefix=DEPS --input-file=%t/main.d -DTMP=%t
// DEPS: [[TMP]]{{/|\\}}main.o :

// RUN: %target-swift-frontend \
// RUN:   -c -o %t/main-2.o -cache-compile-job -cas-path %t/cas -Rcache-compile-job \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd -emit-dependencies -emit-dependencies-path %t/main-2.d 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT

// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}main-2.d': key 'llvmcas://{{.*}}'

// RUN: %FileCheck %s --check-prefix=DEPS2 --input-file=%t/main-2.d -DTMP=%t
// DEPS2: [[TMP]]{{/|\\}}main-2.o :

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps-1.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include -sdk %t/sdk \
// RUN:   -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp -scanner-prefix-map-paths %t/sdk /^sdk
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json A > %t/A1.cmd
// RUN: %swift_frontend_plain @%t/A1.cmd
// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps-1.json > %t/map-1.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map-1.json > %t/map-1.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json Test > %t/MyApp-1.cmd

// RUN: %target-swift-frontend \
// RUN:   -c -o %t/main-3.o -cache-compile-job -cas-path %t/cas -Rcache-compile-job \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp-1.cmd -emit-dependencies -emit-dependencies-path %t/main-3.d

// RUN: %FileCheck %s --check-prefix=DEPS3 --input-file=%t/main-3.d -DTMP=%t
// DEPS3: [[TMP]]{{/|\\}}main-3.o : [[TMP]]{{/|\\}}main.swift

/// Test replay from driver interface
// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 0 -- \
// RUN:   %target-swift-frontend \
// RUN:   -c -o %t/main-4.o -cache-compile-job -cas-path %t/cas -Rcache-compile-job \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp-1.cmd -emit-dependencies -emit-dependencies-path %t/main-4.d > %t/key.casid

// RUN: %swift-scan-test -action replay_result -cas-path %t/cas -id @%t/key.casid -- \
// RUN:   %target-swift-frontend \
// RUN:   -c -o %t/main-4.o -cache-compile-job -cas-path %t/cas -Rcache-compile-job \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -cache-replay-prefix-map /^src %swift_src_root -cache-replay-prefix-map /^tmp %t -cache-replay-prefix-map /^sdk %t/sdk \
// RUN:   /^tmp/main.swift @%t/MyApp-1.cmd -emit-dependencies -emit-dependencies-path %t/main-4.d 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT4
// CACHE-HIT4: remark: replay output file '{{.*}}{{/|\\}}main-4.d': key 'llvmcas://{{.*}}'
// RUN: %FileCheck %s --check-prefix=DEPS4 --input-file=%t/main-4.d -DTMP=%t
// DEPS4: [[TMP]]{{/|\\}}main-4.o : [[TMP]]{{/|\\}}main.swift


//--- main.swift
import A

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func a() { }

