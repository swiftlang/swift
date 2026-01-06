/// Point to the most relevant import relative to the target decl.
// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

/// Build the Swift libraries.
// RUN: %target-swift-frontend -emit-module %t/SwiftPublicNameCore.swift -o %t \
// RUN:   -public-module-name SwiftPublicName
// RUN: %target-swift-frontend -emit-module %t/SwiftPublicName.swift -o %t -I %t
// RUN: %target-swift-frontend -emit-module %t/MixedDep.swift -o %t -I %t

/// Client testing order between indirect imports of the same priority.
// RUN: %target-swift-frontend -typecheck -I %t \
// RUN:   %t/OrderClient_FileA.swift %t/OrderClient_FileB.swift \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -Rmodule-api-import -verify

/// Client testing order of preference for more levels of imports.
// RUN: %target-swift-frontend -typecheck -I %t \
// RUN:   %t/ExportedClient_FileExported.swift %t/ExportedClient_FileA.swift \
// RUN:   %t/ExportedClient_FileB.swift %t/ExportedClient_FileC.swift \
// RUN:   %t/ExportedClient_FileD_via_underlying.swift \
// RUN:   -import-underlying-module -module-name ExportedClient \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -Rmodule-api-import -verify

// Same without the underlying clang module.
// RUN: %target-swift-frontend -typecheck -I %t \
// RUN:   %t/ExportedClient_FileExported.swift %t/ExportedClient_FileA.swift \
// RUN:   %t/ExportedClient_FileB.swift %t/ExportedClient_FileC.swift \
// RUN:   %t/ExportedClient_FileD_via_exported.swift \
// RUN:   -module-name ExportedClient \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -Rmodule-api-import -verify

/// Client testing -public-module-name ordering.
// RUN: %target-swift-frontend -typecheck -I %t \
// RUN:   %t/SwiftLibClient_FileA.swift %t/SwiftLibClient_FileB.swift \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -Rmodule-api-import -verify

/// Client testing import of the overlay vs an unrelated module.
// RUN: %target-swift-frontend -typecheck -I %t \
// RUN:   %t/OverlayClient_FileA.swift %t/OverlayClient_FileB.swift \
// RUN:   -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -Rmodule-api-import -verify

// REQUIRES: swift_feature_InternalImportsByDefault

//--- module.modulemap
module FarClangDep {
    header "FarClangDep.h"
}

module MixedDep {
    export *
    header "MixedDep.h"
}

module IndirectClangDepA {
    export *
    header "IndirectClangDepA.h"
}

module IndirectClangDepB {
    export *
    header "IndirectClangDepB.h"
}

module LibCore {
    export *
    export_as Lib
    header "LibCore.h"
}

module Lib {
    export *
    header "Lib.h"
}

module NotLib {
    export *
    header "NotLib.h"
}

module ExportedClient {
    export *
    header "ExportedClient.h"
}

//--- FarClangDep.h
struct FarClangType{};

//--- MixedDep.h
struct UnderlyingType{};

//--- IndirectClangDepA.h
#include <FarClangDep.h>
#include <MixedDep.h>

//--- IndirectClangDepB.h
#include <FarClangDep.h>
#include <MixedDep.h>

//--- LibCore.h
struct ExportedType {};

//--- Lib.h
#include <LibCore.h>

//--- NotLib.h
#include <LibCore.h>

//--- ExportedClient.h
#include <LibCore.h>

//--- SwiftPublicNameCore.swift
public struct SwiftStruct {}

//--- SwiftPublicName.swift
@_exported import SwiftPublicNameCore

//--- MixedDep.swift
@_exported import MixedDep

//--- OrderClient_FileA.swift
/// Between indirect imports, prefer the first one.
public import IndirectClangDepA
public import IndirectClangDepB // expected-warning {{public import of 'IndirectClangDepB' was not used in public declarations or inlinable code}}

public func useTypesB(a: FarClangType) {}
// expected-remark @-1 {{struct 'FarClangType' is imported via 'IndirectClangDepA', which reexports definition from 'FarClangDep'}}

//--- OrderClient_FileB.swift
/// Still prefer the first one after changing the order.
public import IndirectClangDepB
public import IndirectClangDepA // expected-warning {{public import of 'IndirectClangDepA' was not used in public declarations or inlinable code}}

public func useTypesC(a: FarClangType) {}
// expected-remark @-1 {{struct 'FarClangType' is imported via 'IndirectClangDepB', which reexports definition from 'FarClangDep'}}


//--- ExportedClient_FileExported.swift
@_exported public import ExportedClient

//--- ExportedClient_FileA.swift
/// Prefer the defining module.
public import NotLib // expected-warning {{public import of 'NotLib' was not used in public declarations or inlinable code}}
public import LibCore // We should warn here.
public import Lib

public func useTypesA(a: ExportedType) {}
// expected-remark @-1 {{struct 'ExportedType' is imported via 'Lib', which reexports definition from 'LibCore'}}

//--- ExportedClient_FileB.swift
/// Then prefer the export_as module.
public import NotLib // expected-warning {{public import of 'NotLib' was not used in public declarations or inlinable code}}
public import Lib

public func useTypesB(a: ExportedType) {}
// expected-remark @-1 {{struct 'ExportedType' is imported via 'Lib'}}

//--- ExportedClient_FileC.swift
/// Then prefer any local import.
public import NotLib

public func useTypesC(a: ExportedType) {}
// expected-remark @-1 {{struct 'ExportedType' is imported via 'NotLib', which reexports definition from 'LibCore'}}

//--- ExportedClient_FileD_via_underlying.swift
/// Then use the import of the underling clang module.
public func useTypesD(a: ExportedType) {}
// expected-remark @-1 {{struct 'ExportedType' is imported via 'ExportedClient', which reexports definition from 'LibCore'}}

//--- ExportedClient_FileD_via_exported.swift
/// Finally use the @_exported import from the local module.
public func useTypesD(a: ExportedType) {}
// It would be nice to have a remark even without an import to point to.


//--- SwiftLibClient_FileA.swift
/// Prefer the import matching public-module-name.
public import SwiftPublicNameCore // We should warn here.
public import SwiftPublicName

public func useTypesA(a: SwiftStruct) {}
// expected-remark @-1 {{struct 'SwiftStruct' is imported via 'SwiftPublicName', which reexports definition from 'SwiftPublicName'}}

//--- SwiftLibClient_FileB.swift
/// Fallback on read definition site.
public import SwiftPublicNameCore

public func useTypesB(a: SwiftStruct) {}
// expected-remark @-1 {{struct 'SwiftStruct' is imported via 'SwiftPublicName'}}


//--- OverlayClient_FileA.swift
/// Prefer a Swift overlay to an unrelated 3rd module.
public import IndirectClangDepA // expected-warning {{public import of 'IndirectClangDepA' was not used in public declarations or inlinable code}}
public import MixedDep

public func useTypesA(a: UnderlyingType) {}
// expected-remark @-1 {{struct 'UnderlyingType' is imported via 'MixedDep', which reexports definition from 'MixedDep'}}

//--- OverlayClient_FileB.swift
/// Fallback on any reexporter.
public import IndirectClangDepA

public func useTypesB(a: UnderlyingType) {}
// expected-remark @-1 {{struct 'UnderlyingType' is imported via 'IndirectClangDepA', which reexports definition from 'MixedDep'}}
