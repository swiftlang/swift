// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Check path remapping.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O -import-objc-header %t/objc.h -auto-bridging-header-chaining \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -scanner-prefix-map %t=/^test -scanner-output-dir %t.noremap

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/objc.pch 2>&1 | %FileCheck %s -check-prefix BRIDGE
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/objc.pch > %t/keys.json
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action render-diags %t/keys.json -- \
// RUN:    %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/objc.pch -cache-replay-prefix-map /^test=%t 2>&1 \
// RUN:    | %FileCheck %s -check-prefix BRIDGE -check-prefix BRIDGE-REMAP

// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/objc.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/MyApp.cmd \
// RUN:   -emit-module -o %t/test.swiftmodule /^test/test.swift 2>&1 | %FileCheck %s
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- %target-swift-frontend -cache-compile-job -module-name Test \
// RUN:   -O -cas-path %t/cas @%t/MyApp.cmd \
// RUN:   -emit-module -o %t/test.swiftmodule /^test/test.swift > %t/cache_key.json
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action render-diags %t/cache_key.json -- %target-swift-frontend -cache-compile-job -module-name Test \
// RUN:   -O -cas-path %t/cas @%t/MyApp.cmd \
// RUN:   -emit-module -o %t/test.swiftmodule /^test/test.swift -cache-replay-prefix-map /^test=%t 2>&1 | %FileCheck %s --check-prefix REMAP

// BRIDGE: /^test/objc.h:3:2: warning: warning in bridging header
// BRIDGE-REMAP: BUILD_DIR{{.*}}{{/|\\}}objc.h:3:2: warning: warning in bridging header
// CHECK: /^test/test.swift:1:10: warning: this is a warning
// REMAP: BUILD_DIR{{.*}}{{/|\\}}test.swift:1:10: warning: this is a warning

//--- test.swift
#warning("this is a warning")

//--- objc.h
int test(int);

#warning warning in bridging header
