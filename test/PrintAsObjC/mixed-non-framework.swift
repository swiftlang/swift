// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Relative to the module directory: the include keeps the "sub/" prefix.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name MixedNonFramework -import-underlying-module -I %t/MixedNonFramework -parse-as-library %t/use.swift -typecheck -verify -emit-objc-header-path %t/module-relative.h -generated-header-underlying-module-include-base %t/MixedNonFramework
// RUN: %FileCheck %s < %t/module-relative.h
// RUN: %check-in-clang -I %t/MixedNonFramework %t/module-relative.h

// Relative to the "sub" directory: the include is just "impl.h".
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name MixedNonFramework -import-underlying-module -I %t/MixedNonFramework -parse-as-library %t/use.swift -typecheck -verify -emit-objc-header-path %t/subdir-relative.h -generated-header-underlying-module-include-base %t/MixedNonFramework/sub
// RUN: %FileCheck %s --check-prefix=SUBDIR < %t/subdir-relative.h
// RUN: %check-in-clang -I %t/MixedNonFramework/sub %t/subdir-relative.h

// REQUIRES: objc_interop

// CHECK: #import "sub/impl.h"
// CHECK-NOT: #import <MixedNonFramework/MixedNonFramework.h>

// SUBDIR: #import "impl.h"
// SUBDIR-NOT: #import <MixedNonFramework/MixedNonFramework.h>

//--- MixedNonFramework/module.modulemap
module MixedNonFramework {
  header "sub/impl.h"
  export *
}

//--- MixedNonFramework/sub/impl.h
typedef int MixedNonFrameworkInt;

//--- use.swift
import Foundation

public class SomeClass: NSObject {
  @objc public func getValue() -> MixedNonFrameworkInt {
    return 0
  }
}
