// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/cas
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-cache-path %t/clang-module-cache %t/A.swift -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -o %t/A.swiftmodule -swift-version 5
// RUN: %target-swift-frontend -emit-module -module-cache-path %t/clang-module-cache %t/B.swift -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -o %t/B.swiftmodule -I %t -swift-version 5

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %t/Test.swift -o %t/deps.json -I %t -module-name Test \
// RUN:    -swift-version 5 -cache-compile-job -cas-path %t/cas -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib
// RUN: %validate-json %t/deps.json &>/dev/null

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/Test.swift @%t/MyApp.cmd

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Foo.swiftmodule -emit-module-interface-path %t/Foo.swiftinterface -disable-implicit-swift-modules \
// RUN:   -module-cache-path %t.module-cache -explicit-swift-module-map-file @%t/map.casid %t/Test.swift -cache-compile-job \
// RUN:   -cas-path %t/cas -swift-version 5 -enable-library-evolution -o %t/Foo.swiftmodule \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib @%t/MyApp.cmd
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend -emit-module -emit-module-path %t/Foo.swiftmodule -emit-module-interface-path %t/Foo.swiftinterface -disable-implicit-swift-modules \
// RUN:   -module-cache-path %t.module-cache -explicit-swift-module-map-file @%t/map.casid %t/Test.swift -cache-compile-job \
// RUN:   -cas-path %t/cas -swift-version 5 -enable-library-evolution -o %t/Foo.swiftmodule \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib @%t/MyApp.cmd > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json %t/Test.swift > %t/key

// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-compile-cache-key @%t/key | %FileCheck %s --check-prefix=CACHE-KEY
// CACHE-KEY: Cache Key llvmcas://
// CACHE-KEY-NEXT: Swift Compiler Invocation Info:
// CACHE-KEY-NEXT: command-line
// CACHE-KEY: Input index: 0

// RUN: %target-swift-frontend -typecheck-module-from-interface %t/Foo.swiftinterface -disable-implicit-swift-modules \
// RUN:   -module-cache-path %t.module-cache -explicit-swift-module-map-file @%t/map.casid  \
// RUN:   -cache-compile-job -cas-path %t/cas -swift-version 5 -enable-library-evolution \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -explicit-interface-module-build -Rcache-compile-job @%t/MyApp.cmd -input-file-key @%t/key 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=CACHE-MISS
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/Foo.swiftinterface -disable-implicit-swift-modules \
// RUN:   -module-cache-path %t.module-cache -explicit-swift-module-map-file @%t/map.casid  \
// RUN:   -cache-compile-job -cas-path %t/cas -swift-version 5 -enable-library-evolution \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -explicit-interface-module-build -Rcache-compile-job @%t/MyApp.cmd -input-file-key @%t/key 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=CACHE-HIT

// CACHE-MISS: remark: cache miss for input
// VERIFY-OUTPUT: warning: module 'B' was not compiled with library evolution support
// CACHE-HIT: remark: replay output file

//--- A.swift
func test() {}

//--- B.swift
import A
func myTest() {}

//--- Test.swift
import B


