// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/tmp

// Test with the normal bridging header.
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -verify-additional-prefix non-library-evolution- -verify-additional-prefix ctypes-not-imported- -verify-additional-prefix no-import-

// RUN: not %target-swift-frontend -typecheck -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk %s 2>&1 | %FileCheck %s

// Test with a precompiled bridging header.
// RUN: %target-swift-frontend -emit-pch -o %t/c-bridging-header.pch %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -swift-version 5
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %t/c-bridging-header.pch -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -verify-additional-prefix non-library-evolution- -verify-additional-prefix ctypes-not-imported- -verify-additional-prefix no-import-

// Test the spelling of the suggested import with InternalImportsByDefault.
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -verify-additional-prefix non-library-evolution- -verify-additional-prefix ctypes-not-imported- -verify-additional-prefix internal-imports-by-default- -enable-upcoming-feature InternalImportsByDefault

// Test that any import of a module reached through the bridging header, even
// a less permissive one, makes it a dependency for layouts exposed to clients.
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -verify-additional-prefix non-library-evolution- -D FILEPRIVATE_IMPORT

// Test that importing another module with a bridging header doesn't count as
// an import of what this bridging header declares or includes, even though
// all bridging headers share one header module.
// RUN: echo 'typedef int OtherInt;' > %t/other-bridging-header.h
// RUN: echo 'public func otherFunc() {}' > %t/HasBridgingHeader.swift
// RUN: %target-swift-frontend -emit-module -o %t -module-name HasBridgingHeader %t/HasBridgingHeader.swift -import-objc-header %t/other-bridging-header.h -sdk %clang-importer-sdk -swift-version 5
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -verify-additional-prefix non-library-evolution- -verify-additional-prefix ctypes-not-imported- -verify-additional-prefix other-module- -I %t -D OTHER_MODULE_IMPORT

// Test that a module imported only implementation-only by another module isn't
// an import of it, since that other module's clients don't load it.
// RUN: echo '@_implementationOnly import ctypes' > %t/ImplOnlyImportsCTypes.swift
// RUN: %target-swift-frontend -emit-module -o %t -module-name ImplOnlyImportsCTypes %t/ImplOnlyImportsCTypes.swift -sdk %clang-importer-sdk -swift-version 5
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -verify-additional-prefix non-library-evolution- -verify-additional-prefix ctypes-not-imported- -verify-additional-prefix impl-only-via-other- -I %t -D IMPL_ONLY_VIA_OTHER_MODULE

// Test library-evolution differences.
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -enable-library-evolution

// Verify with SerializeAbstractTypeLayoutForHiddenTypes enabled, diagnostics are silenced, as if library evolution was enabled
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: swift_feature_InternalImportsByDefault

#if FILEPRIVATE_IMPORT
fileprivate import ctypes
#elseif OTHER_MODULE_IMPORT
fileprivate import HasBridgingHeader // expected-other-module-warning {{implicit import of bridging header 'other-bridging-header.h' via module 'HasBridgingHeader' is deprecated}}
// expected-other-module-note 2 {{add import of module 'ctypes'}}{{1-1=internal import ctypes\n}}
#elseif IMPL_ONLY_VIA_OTHER_MODULE
import ImplOnlyImportsCTypes
// expected-impl-only-via-other-note 2 {{add import of module 'ctypes'}}{{1-1=internal import ctypes\n}}
#endif

// Uses of 'ctypes' declarations below suggest an import, inserted after the
// imports or before the first declaration.
// expected-no-import-note@+2 2 {{add import of module 'ctypes'}}{{1-1=internal import ctypes\n}}
// expected-internal-imports-by-default-note@+1 2 {{add import of module 'ctypes'}}{{1-1=import ctypes\n}}
@inlinable
public func f() -> Any {
  return red
  // expected-error@-1{{var 'red' is internal and cannot be referenced from an '@inlinable' function}}
  // expected-error@-2{{getter for var 'red' is internal and cannot be referenced from an '@inlinable' function}}
}

// CHECK: var 'red' imported as 'internal' from bridging header

@c @implementation
public func cImpl(_ a: MyPoint) { } // No error on public reference to `MyPoint`

@frozen
public struct FrozenStruct {
    var p: MyPoint // expected-error {{type referenced from a stored property in a '@frozen' struct must be '@usableFromInline' or public}}
    // expected-note @-1 {{struct 'MyPoint' is imported by this file as 'internal' from bridging header}}
}

public struct PublicStruct {
    var p: MyPoint // expected-non-library-evolution-error {{cannot use struct 'MyPoint' in a property declaration member of a type not marked '@_implementationOnly'; it was imported via the internal bridging header}}
}

internal struct InternalStruct {
    var p: MyPoint // expected-non-library-evolution-error {{cannot use struct 'MyPoint' in a property declaration member of a type not marked '@_implementationOnly'; it was imported via the internal bridging header}}
}

// 'Point' is declared by the 'ctypes' module that the bridging header includes.
internal struct InternalStructFromModule {
    var p: Point // expected-ctypes-not-imported-error {{cannot use struct 'Point' in a property declaration member of a type not marked '@_implementationOnly'; it was imported via the internal bridging header}}
}

typealias PointAlias = Point

internal struct InternalStructFromModuleViaAlias {
    var p: PointAlias // expected-ctypes-not-imported-warning {{'PointAlias' aliases 'ctypes.Point' and cannot be used in a property declaration member of a type not marked '@_implementationOnly' because it was imported via the internal bridging header; this is an error in the Swift 6 language mode}}
}
