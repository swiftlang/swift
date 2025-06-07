// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/Test.swiftmodule \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -import-objc-header %t/Bridging.h

// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps.json \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -I %t

/// Remove bridging header from disk and rescan
// RUN: rm -rf %t/Bridging.h %t/Foo.h %t/Foo2.h
// RUN: %target-swift-frontend -scan-dependencies -module-name User -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/user.swift -o %t/deps2.json \
// RUN:   -Xcc -fmodule-map-file=%t/a.modulemap -Xcc -fmodule-map-file=%t/b.modulemap -I %t

// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json swiftPrebuiltExternal:Test headerModuleDependencies | %FileCheck %s --check-prefix=MODULE
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json swiftPrebuiltExternal:Test headerDependenciesSourceFiles | %FileCheck %s --check-prefix=FILE
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps2.json swiftPrebuiltExternal:Test headerModuleDependencies | %FileCheck %s --check-prefix=MODULE
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps2.json swiftPrebuiltExternal:Test headerDependenciesSourceFiles | %FileCheck %s --check-prefix=FILE

// MODULE: "A"
// FILE: Bridging.h

//--- test.swift
public func test() {
    b()
}
public class TestB: B {}

//--- user.swift
import Test

func user() {
  var b: TestB
  test()
}

extension A {
    public func testA() {}
}

//--- Bridging.h
#include "Foo.h"
#include "Foo2.h"

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
