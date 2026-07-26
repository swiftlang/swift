// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/source1/test.swift -o %t/deps-1.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %t/source1 /^src \
// RUN:   -scanner-output-dir %t -import-objc-header %t/source1/Bridging.h -I %t/source1

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps-1.json -o %t/Test.cmd -b %t/header.cmd

// RUN: %target-swift-frontend-plain @%t/header.cmd /^src/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend-plain @%t/header.cmd /^src/Bridging.h -disable-implicit-swift-modules -O -o %t/bridging.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: %target-swift-frontend-plain -cache-compile-job -module-name Test -O -cas-path %t/cas @%t/Test.cmd /^src/test.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -import-objc-header /^src/Bridging.h -import-pch %t/bridging.pch -bridging-header-pch-key @%t/key \
// RUN:   -emit-module -o %t/Test.swiftmodule

// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/source2/user.swift -o %t/deps-2.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %t/source2 /^src \
// RUN:   -scanner-output-dir %t -I %t -I %t/source1 2>&1 | %FileCheck %s --check-prefix NO-ERROR --allow-empty

// NO-ERROR-NOT: error:

//--- source1/test.swift
public func test() {
    bridge()
}

//--- source2/user.swift
import Test

//--- source1/Bridging.h
#include "A.h"

//--- source1/module.modulemap
module A {
  header "A.h"
  export *
}

//--- source1/A.h
void bridge(void);
