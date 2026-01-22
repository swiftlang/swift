// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/tmp

// Test with the normal bridging header.
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -verify-additional-prefix non-library-evolution-

// RUN: not %target-swift-frontend -typecheck -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk %s 2>&1 | %FileCheck %s

// Test with a precompiled bridging header.
// RUN: %target-swift-frontend -emit-pch -o %t/c-bridging-header.pch %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -swift-version 5
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %t/c-bridging-header.pch -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -verify-additional-prefix non-library-evolution-

// Test library-evolution differences.
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -verify-ignore-unknown -swift-version 5 -enable-library-evolution

@inlinable
public func f() -> Any {
  return red
  // expected-error@-1{{var 'red' is internal and cannot be referenced from an '@inlinable' function}}
  // expected-error@-2{{getter for var 'red' is internal and cannot be referenced from an '@inlinable' function}}
}

// CHECK: var 'red' imported as 'internal' from bridging header

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
