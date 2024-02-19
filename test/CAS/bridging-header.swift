// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -import-objc-header %t/Bridging.h

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test bridgingHeader | %FileCheck %s

// CHECK:       "includeTree"
// CHECK-NEXT:  "moduleDependencies": [
// CHECK-NEXT:    "A"
// CHECK-NEXT:  ],
// CHECK-NEXT:  "commandLine": [
// CHECK:         "-fmodule-file-cache-key",
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "{{.*}}{{/|\\}}A-{{.*}}.pcm", 
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "llvmcas://{{.*}}", 
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "-fmodule-file-cache-key",
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "{{.*}}{{/|\\}}B-{{.*}}.pcm", 
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "llvmcas://{{.*}}"

//--- test.swift
import B
public func test() {}

//--- Bridging.h
#include "Foo.h"

//--- Foo.h
#import "a.h"

//--- a.h
#include "b.h"

//--- b.h
void b(void);

//--- a.modulemap
module A {
  header "a.h"
  export *
}

//--- b.modulemap
module B {
  header "b.h"
  export *
}
