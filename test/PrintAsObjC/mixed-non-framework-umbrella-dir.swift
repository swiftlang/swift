// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name UmbrellaDirMod -import-underlying-module -I %t/UmbrellaDirMod -parse-as-library %t/use.swift -typecheck -verify -emit-objc-header-path %t/module-relative.h -generated-header-underlying-module-include-base %t/UmbrellaDirMod
// RUN: %FileCheck %s < %t/module-relative.h
// RUN: %check-in-clang -I %t/UmbrellaDirMod %t/module-relative.h

// REQUIRES: objc_interop

// CHECK: #import "Headers/First.h"
// CHECK-NEXT: #import "Headers/Second.h"
// CHECK-NOT: #import <UmbrellaDirMod/UmbrellaDirMod.h>

//--- UmbrellaDirMod/module.modulemap
module UmbrellaDirMod {
  umbrella "Headers"
  export *
}

//--- UmbrellaDirMod/Headers/First.h
typedef int UmbrellaDirModFirst;

//--- UmbrellaDirMod/Headers/Second.h
typedef int UmbrellaDirModSecond;

//--- use.swift
import Foundation

public class SomeClass: NSObject {
  @objc public func getFirst() -> UmbrellaDirModFirst { return 0 }
  @objc public func getSecond() -> UmbrellaDirModSecond { return 0 }
}
