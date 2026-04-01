// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -auto-bridging-header-chaining -cache-compile-job -cas-path %t/cas \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -import-objc-header %t/Bridging.h

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -b %t/Test-bridging.cmd -o %t/Test.cmd

// RUN: %target-swift-frontend-plain @%t/Test-bridging.cmd %t/Bridging.h -disable-implicit-swift-modules -o %t/objc.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend-plain @%t/Test-bridging.cmd %t/Bridging.h -disable-implicit-swift-modules -o %t/objc.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: echo "\"-import-objc-header\"" >> %t/Test.cmd
// RUN: echo "\"%t/Bridging.h\"" >> %t/Test.cmd
// RUN: echo "\"-import-pch\"" >> %t/Test.cmd
// RUN: echo "\"%t/objc.pch\"" >> %t/Test.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/Test.cmd
// RUN: echo "\"@%t/key\"" >> %t/Test.cmd

// RUN: %target-swift-frontend-plain  -cache-compile-job -module-name Test -g -cas-path %t/cas @%t/Test.cmd \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -emit-module-path %t/Test.swiftmodule %t/test.swift -debug-module-self-key -emit-ir -o %t/test.ll

// RUN: %FileCheck %s --input-file %t/test.ll

// CHECK-DAG: !DIModule(scope: null, name: "Test", includePath: "llvmcas://{{.*}}")
// CHECK-DAG: !DIModule(scope: null, name: "__ObjC", configMacros: "{{.*}}", includePath: "llvmcas://{{.*}}")
// CHECK-DAG: !DICompileUnit(language: DW_LANG_ObjC, file: {{.*}}, producer: {{.*}}, splitDebugFilename: "llvmcas://{{.*}}"

//--- test.swift
public func test() {
    b()
    let test = Bridge()
}
public class TestB: B {}

//--- Bridging.h
#include "Foo.h"
#include "Foo2.h"

struct Bridge {};

//--- Foo.h
#import "a.h"
#ifndef IMPORT_FOO
#define IMPORT_FOO
int Foo = 0;
#endif

//--- Foo2.h
#ifndef IMPORT_FOO2
#define IMPORT_FOO2
int Foo2 = 0;
#endif

//--- a.h
#include "b.h"
struct A {
  int a;
};

//--- b.h
void b(void);
@interface B
@end

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
