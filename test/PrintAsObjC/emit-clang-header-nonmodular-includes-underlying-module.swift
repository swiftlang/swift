// REQUIRES: objc_interop

// When the Swift half of a mixed module references its own Objective-C half,
// -emit-clang-header-nonmodular-includes imports the underlying module's
// headers directly, spelled relative to the header search paths, instead of
// assuming a <Module/Module.h> umbrella header exists.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Individual headers, including submodule and textual headers. The headers
// are sorted, and explicit submodules are not imported.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name HeaderList -import-underlying-module -Xcc -fmodule-map-file=%t/modulemaps/HeaderList.modulemap -I %t -parse-as-library %t/HeaderList.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/header-list.h
// RUN: %FileCheck %s --check-prefix=HEADER-LIST < %t/header-list.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments -I %t %t/header-list.h

// HEADER-LIST-NOT: @import HeaderList;
// HEADER-LIST-NOT: #import <HeaderList/HeaderList.h>
// HEADER-LIST:      #import <src/HeaderList.h>
// HEADER-LIST-NEXT: #import <src/HeaderListTextual.h>
// HEADER-LIST-NEXT: #import <src/sub/HeaderListOther.h>
// HEADER-LIST-EMPTY:
// HEADER-LIST-NOT: #import <HeaderList/HeaderList.h>
// HEADER-LIST-NOT: HeaderListPrivate.h
// HEADER-LIST-DAG: @interface HeaderListObjc (SWIFT_EXTENSION(HeaderList))
// HEADER-LIST-DAG: @interface SwiftOther : HeaderListOther

// Without -emit-clang-header-nonmodular-includes, the umbrella header import
// is unchanged.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name HeaderList -import-underlying-module -Xcc -fmodule-map-file=%t/modulemaps/HeaderList.modulemap -I %t -parse-as-library %t/HeaderList.swift -typecheck -verify -emit-objc-header-path %t/header-list-modular.h
// RUN: %FileCheck %s --check-prefix=UMBRELLA-HEADER-LIST < %t/header-list-modular.h

// UMBRELLA-HEADER-LIST: #import <HeaderList/HeaderList.h>
// UMBRELLA-HEADER-LIST-NOT: #import <src/
// UMBRELLA-HEADER-LIST: @interface HeaderListObjc (SWIFT_EXTENSION(HeaderList))

// -generated-header-underlying-module-include-base takes precedence and keeps
// using quoted includes relative to the given base.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name HeaderList -import-underlying-module -Xcc -fmodule-map-file=%t/modulemaps/HeaderList.modulemap -I %t -parse-as-library %t/HeaderList.swift -typecheck -verify -emit-clang-header-nonmodular-includes -generated-header-underlying-module-include-base %t/src -emit-objc-header-path %t/header-list-include-base.h
// RUN: %FileCheck %s --check-prefix=INCLUDE-BASE < %t/header-list-include-base.h

// INCLUDE-BASE: #import "HeaderList.h"
// INCLUDE-BASE-NEXT: #import "HeaderListTextual.h"
// INCLUDE-BASE-NEXT: #import "sub/HeaderListOther.h"
// INCLUDE-BASE-NOT: #import <src/
// INCLUDE-BASE-NOT: #import <HeaderList/HeaderList.h>

// If the Swift half doesn't reference the Objective-C half, nothing from the
// underlying module is imported.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name HeaderList -import-underlying-module -Xcc -fmodule-map-file=%t/modulemaps/HeaderList.modulemap -I %t -parse-as-library %t/NoReference.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/no-reference.h
// RUN: %FileCheck %s --check-prefix=NO-REFERENCE < %t/no-reference.h

// NO-REFERENCE-NOT: #import <src/
// NO-REFERENCE-NOT: #import <HeaderList/HeaderList.h>
// NO-REFERENCE: @interface SwiftOnly : NSObject
// NO-REFERENCE-NOT: #import <src/
// NO-REFERENCE-NOT: #import <HeaderList/HeaderList.h>

// The C section for @c declarations also imports the headers directly with
// the flag, and keeps the umbrella header without it.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name HeaderList -import-underlying-module -Xcc -fmodule-map-file=%t/modulemaps/HeaderList.modulemap -I %t -parse-as-library %t/CDecl.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/cdecl.h
// RUN: %FileCheck %s --check-prefix=CDECL < %t/cdecl.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name HeaderList -import-underlying-module -Xcc -fmodule-map-file=%t/modulemaps/HeaderList.modulemap -I %t -parse-as-library %t/CDecl.swift -typecheck -verify -emit-objc-header-path %t/cdecl-modular.h
// RUN: %FileCheck %s --check-prefix=UMBRELLA-CDECL < %t/cdecl-modular.h

// CDECL-NOT: #import <HeaderList/HeaderList.h>
// CDECL: #import <src/HeaderList.h>
// CDECL-NEXT: #import <src/HeaderListTextual.h>
// CDECL-NEXT: #import <src/sub/HeaderListOther.h>
// CDECL-NOT: #import <HeaderList/HeaderList.h>
// CDECL: SWIFT_EXTERN HeaderListTextualInt cdecl_textual_int(void)

// UMBRELLA-CDECL: #import <HeaderList/HeaderList.h>
// UMBRELLA-CDECL-NOT: #import <src/
// UMBRELLA-CDECL: SWIFT_EXTERN HeaderListTextualInt cdecl_textual_int(void)

// A non-framework umbrella header is imported through its own path. The
// longest matching search path determines the spelling.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name UmbrellaHeader -import-underlying-module -I %t -parse-as-library %t/UmbrellaHeader.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/umbrella-header.h
// RUN: %FileCheck %s --check-prefix=UMBRELLA-HEADER < %t/umbrella-header.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments -I %t %t/umbrella-header.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name UmbrellaHeader -import-underlying-module -I %t -Xcc -I%t/UmbrellaHeader/include -parse-as-library %t/UmbrellaHeader.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/umbrella-header-nested.h
// RUN: %FileCheck %s --check-prefix=UMBRELLA-HEADER-NESTED < %t/umbrella-header-nested.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments -I %t/UmbrellaHeader/include %t/umbrella-header-nested.h

// UMBRELLA-HEADER-NOT: #import <UmbrellaHeader/UmbrellaHeader.h>
// UMBRELLA-HEADER: #import <UmbrellaHeader/include/UmbrellaHeaderAll.h>
// UMBRELLA-HEADER-NOT: #import <UmbrellaHeader/UmbrellaHeader.h>
// UMBRELLA-HEADER: @interface UmbrellaHeaderObjc (SWIFT_EXTENSION(UmbrellaHeader))

// UMBRELLA-HEADER-NESTED-NOT: #import <UmbrellaHeader/
// UMBRELLA-HEADER-NESTED: #import <UmbrellaHeaderAll.h>
// UMBRELLA-HEADER-NESTED-NOT: #import <UmbrellaHeader/
// UMBRELLA-HEADER-NESTED: @interface UmbrellaHeaderObjc (SWIFT_EXTENSION(UmbrellaHeader))

// Every header found in an umbrella directory, including nested ones, is
// imported.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name UmbrellaDir -import-underlying-module -I %t -parse-as-library %t/UmbrellaDir.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/umbrella-dir.h
// RUN: %FileCheck %s --check-prefix=UMBRELLA-DIR < %t/umbrella-dir.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments -I %t %t/umbrella-dir.h

// UMBRELLA-DIR-NOT: #import <UmbrellaDir/UmbrellaDir.h>
// UMBRELLA-DIR: #import <UmbrellaDir/Headers/UmbrellaDirA.h>
// UMBRELLA-DIR-NEXT: #import <UmbrellaDir/Headers/UmbrellaDirB.h>
// UMBRELLA-DIR-NEXT: #import <UmbrellaDir/Headers/nested/UmbrellaDirNested.h>
// UMBRELLA-DIR-NOT: #import <UmbrellaDir/UmbrellaDir.h>
// UMBRELLA-DIR: @interface UmbrellaDirA (SWIFT_EXTENSION(UmbrellaDir))

// If the module's headers are not on any search path, they cannot be spelled
// with angle brackets, so fall back to the umbrella header.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name Outside -import-underlying-module -Xcc -fmodule-map-file=%t/outside/Outside.modulemap -parse-as-library %t/Outside.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/outside.h
// RUN: %FileCheck %s --check-prefix=OUTSIDE < %t/outside.h

// OUTSIDE: #import <Outside/Outside.h>
// OUTSIDE-NOT: OutsideObjc.h>
// OUTSIDE: @interface OutsideObjc (SWIFT_EXTENSION(Outside))

// The same happens if only some of the headers are on a search path, rather
// than importing an incomplete set of headers.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name Partial -import-underlying-module -Xcc -fmodule-map-file=%t/partial/Partial.modulemap -Xcc -I%t/partial/inside -parse-as-library %t/Partial.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/partial.h
// RUN: %FileCheck %s --check-prefix=PARTIAL < %t/partial.h

// PARTIAL: #import <Partial/Partial.h>
// PARTIAL-NOT: PartialInside.h>
// PARTIAL-NOT: PartialOutside.h>
// PARTIAL: @interface PartialInside (SWIFT_EXTENSION(Partial))

// A bridging header is still imported as before.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name Bridged -import-objc-header %t/src/Bridged.h -I %t -parse-as-library %t/Bridged.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/bridged.h
// RUN: %FileCheck %s --check-prefix=BRIDGED < %t/bridged.h

// BRIDGED-NOT: #import <src/Bridged.h>
// BRIDGED: #import "{{.*}}src/Bridged.h"
// BRIDGED-NEXT: #else
// BRIDGED-NEXT: #include "{{.*}}src/Bridged.h"
// BRIDGED-NEXT: #endif
// BRIDGED-NOT: #import <src/Bridged.h>
// BRIDGED-NOT: #import <Bridged/Bridged.h>
// BRIDGED: @interface BridgedObjc (SWIFT_EXTENSION(Bridged))

// A framework's umbrella header is spelled through the framework search path,
// which is the same as the umbrella header assumed without the flag.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name Mixed -import-underlying-module -F %S/Inputs -parse-as-library %t/Framework.swift -typecheck -verify -emit-clang-header-nonmodular-includes -emit-objc-header-path %t/framework.h
// RUN: %FileCheck %s --check-prefix=FRAMEWORK < %t/framework.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments -F %S/Inputs %t/framework.h

// FRAMEWORK: #import <Mixed/Mixed.h>
// FRAMEWORK-NOT: #import <Mixed/Mixed.h>
// FRAMEWORK-NOT: Mixed.framework
// FRAMEWORK: - (CIntAlias)getIntAlias

//--- modulemaps/HeaderList.modulemap
module HeaderList {
  textual header "../src/HeaderListTextual.h"
  module Other {
    header "../src/sub/HeaderListOther.h"
    export *
  }
  module Main {
    header "../src/HeaderList.h"
    export *
  }
  explicit module Private {
    header "../src/private/HeaderListPrivate.h"
    export *
  }
}

//--- src/HeaderList.h
#import <Foundation.h>
#include "HeaderListTextual.h"

@interface HeaderListObjc : NSObject
@end

//--- src/HeaderListTextual.h
#ifndef HEADER_LIST_TEXTUAL_H
#define HEADER_LIST_TEXTUAL_H
typedef int HeaderListTextualInt;
#endif

//--- src/sub/HeaderListOther.h
#import <Foundation.h>

@interface HeaderListOther : NSObject
@end

//--- src/private/HeaderListPrivate.h
#import <Foundation.h>

@interface HeaderListPrivate : NSObject
@end

//--- HeaderList.swift
import Foundation

extension HeaderListObjc {
  @objc public func swiftValue() -> HeaderListTextualInt { return 1 }
}

public class SwiftOther: HeaderListOther {}

//--- NoReference.swift
import Foundation

public class SwiftOnly: NSObject {
  @objc public func swiftValue() -> Int32 { return 1 }
}

//--- CDecl.swift
@c(cdecl_textual_int)
public func cdeclTextualInt() -> HeaderListTextualInt { return 1 }

//--- UmbrellaHeader/module.modulemap
module UmbrellaHeader {
  umbrella header "include/UmbrellaHeaderAll.h"
  export *
}

//--- UmbrellaHeader/include/UmbrellaHeaderAll.h
#import <Foundation.h>

@interface UmbrellaHeaderObjc : NSObject
@end

//--- UmbrellaHeader.swift
import Foundation

extension UmbrellaHeaderObjc {
  @objc public func swiftValue() -> Int32 { return 1 }
}

//--- UmbrellaDir/module.modulemap
module UmbrellaDir {
  umbrella "Headers"
  export *
}

//--- UmbrellaDir/Headers/UmbrellaDirB.h
typedef int UmbrellaDirBInt;

//--- UmbrellaDir/Headers/UmbrellaDirA.h
#import <Foundation.h>

@interface UmbrellaDirA : NSObject
@end

//--- UmbrellaDir/Headers/nested/UmbrellaDirNested.h
typedef int UmbrellaDirNestedInt;

//--- UmbrellaDir.swift
import Foundation

extension UmbrellaDirA {
  @objc public func b() -> UmbrellaDirBInt { return 1 }
  @objc public func nested() -> UmbrellaDirNestedInt { return 1 }
}

//--- outside/Outside.modulemap
module Outside {
  header "OutsideObjc.h"
  export *
}

//--- outside/OutsideObjc.h
#import <Foundation.h>

@interface OutsideObjc : NSObject
@end

//--- Outside.swift
import Foundation

extension OutsideObjc {
  @objc public func swiftValue() -> Int32 { return 1 }
}

//--- partial/Partial.modulemap
module Partial {
  header "inside/PartialInside.h"
  header "outside/PartialOutside.h"
  export *
}

//--- partial/inside/PartialInside.h
#import <Foundation.h>

@interface PartialInside : NSObject
@end

//--- partial/outside/PartialOutside.h
typedef int PartialOutsideInt;

//--- Partial.swift
import Foundation

extension PartialInside {
  @objc public func swiftValue() -> PartialOutsideInt { return 1 }
}

//--- src/Bridged.h
#import <Foundation.h>

@interface BridgedObjc : NSObject
@end

//--- Bridged.swift
import Foundation

extension BridgedObjc {
  @objc public func swiftValue() -> Int32 { return 1 }
}

//--- Framework.swift
import Foundation

public class SwiftUsesFramework: NSObject {
  @objc public func getIntAlias() -> CIntAlias { return 0 }
}
