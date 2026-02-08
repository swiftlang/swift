// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build Test Module using caching.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/Test.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include
// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/Test.cmd

// RUN: %target-swift-frontend-plain \
// RUN:   -emit-module -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 %t/Test.swift -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -o %t/include/Test.swiftmodule @%t/Test.cmd

/// Build User Module without caching with the swift overlay removed.
// RUN: rm %t/include/B.swiftinterface
// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/User.swift -o %t/deps2.json -swift-version 5 -I %t/include
// RUN: %{python} %S/../../utils/swift-build-modules.py %swift_frontend_plain %t/deps2.json -o %t/User.cmd

// RUN: %target-swift-frontend -typecheck \
// RUN:   -swift-version 5 %t/User.swift -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name User @%t/User.cmd

//--- Test.swift
import B
public func test() {}

//--- User.swift
import Test

func use() {
  test()
}

//--- include/module.modulemap
module A {
  header "A.h"
  export *
}

module B {
  header "B.h"
  export *
}

//--- include/A.h
void notused(void);

//--- include/B.h
#include "A.h"
void notused2(void);

//--- include/B.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name B -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
@_exported import B
public func c() { }
