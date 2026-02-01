// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(demangle_retroactive_equatable_other)) -enable-library-evolution %S/Inputs/demangle_retroactive_equatable_other.swift -emit-module -emit-module-path %t/demangle_retroactive_equatable_other.swiftmodule -module-name demangle_retroactive_equatable_other
// RUN: %target-codesign %t/%target-library-name(demangle_retroactive_equatable_other)

// RUN: %target-build-swift %s -ldemangle_retroactive_equatable_other -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(demangle_retroactive_equatable_other)

// REQUIRES: executable_test

import demangle_retroactive_equatable_other

// rdar://168023786

// When the stdlib's Equatable protocol was changed to suppress ~Copyable
// and ~Escapable on Self, it exposed a mangler bug, and as a result we
// would incorrectly mangle the type 'G<Optional<X>>' below, and crash at
// runtime when attempting to realize its metadata. Make sure that works
// properly.

public struct G<T: Equatable> {}

extension X: @retroactive Equatable {
  public static func ==(_: Self, _: Self) -> Bool { return false }
}

// CHECK: main.G<Swift.Optional<main.X>>
print(G<Optional<X>>.self)
