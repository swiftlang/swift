// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %t/inputs -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -module-name CycleKit  &> %t/out.txt
// RUN: %FileCheck %s < %t/out.txt

// CHECK: error: module dependency cycle: 'CycleKit (Source Target) -> A.swiftinterface -> CycleKit.swiftinterface'
// CHECK: note: Swift Overlay dependency of 'A' on 'CycleKit' via Clang module dependency: 'A.swiftinterface -> A.pcm -> B.pcm -> CycleKit.pcm'

//--- test.swift
import A

//--- inputs/CycleKit.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name CycleKit -enable-library-evolution

public func CycleKitFunc() {}

//--- inputs/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -enable-library-evolution
@_exported import A
public func AFunc() {}

//--- inputs/A.h
#import <B.h>
void funcA(void);

//--- inputs/B.h
#import <CycleKit.h>
void funcA(void);

//--- inputs/CycleKit.h
void funcCycleKit(void);

//--- inputs/module.modulemap
module A {
  header "A.h"
  export *
}

module B {
  header "B.h"
  export *
}

module CycleKit {
  header "CycleKit.h"
  export *
}
