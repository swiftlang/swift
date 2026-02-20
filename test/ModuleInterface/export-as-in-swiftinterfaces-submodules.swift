// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build a client of the Library lib.
// RUN: %target-swift-frontend -emit-module %t/LibraryClient.swift \
// RUN:   -module-name LibraryClient -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/LibraryClient.swiftmodule \
// RUN:   -emit-module-interface-path %t/LibraryClient.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/LibraryClient.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/LibraryClient.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/LibraryClient.private.swiftinterface) -module-name LibraryClient -I %t
// RUN: %FileCheck -input-file=%t/LibraryClient.swiftinterface -check-prefix=PUBLIC %t/LibraryClient.swift
// RUN: %FileCheck -input-file=%t/LibraryClient.private.swiftinterface -check-prefix=PRIVATE %t/LibraryClient.swift

/// Build a client of the LibraryCore lib. Note that the public interface is not
/// expected to be usable--a direct client of a module using `export_as` is
/// eventually folded into the `export_as` module itself.
// RUN: %target-swift-frontend -emit-module %t/LibraryCoreClient.swift \
// RUN:   -module-name LibraryCoreClient -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/LibraryCoreClient.swiftmodule \
// RUN:   -emit-module-interface-path %t/LibraryCoreClient.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/LibraryCoreClient.private.swiftinterface
// RUN: not %target-swift-typecheck-module-from-interface(%t/LibraryCoreClient.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/LibraryCoreClient.private.swiftinterface) -module-name LibraryCoreClient -I %t
// RUN: %FileCheck -input-file=%t/LibraryCoreClient.swiftinterface -check-prefix=PUBLIC %t/LibraryCoreClient.swift
// RUN: %FileCheck -input-file=%t/LibraryCoreClient.private.swiftinterface -check-prefix=PRIVATE %t/LibraryCoreClient.swift

//--- module.modulemap
module LibraryCore {
  export_as Library

  umbrella "LibraryCore"
  header "LibraryCore/LibraryCore.h"
  export *

  explicit module * { export * }
}

module Library {
  umbrella "Library"
  header "Library/Library.h"
  export *

  explicit module * { export * }
}

//--- LibraryCore/LibraryCore.h
// This file is re-exported by Library
#pragma once
struct LibraryCoreType {};

//--- LibraryCore/Submodule.h
// This file is re-exported by Library.Submodule (which Swift does not respect)
#pragma once
struct LibraryCoreSubmoduleType {};

//--- LibraryCore/ReexportedSubmodule.h
// This file is re-exported by Library
#pragma once
struct LibraryCoreReexportedSubmoduleType {};

//--- Library/Library.h
#include <LibraryCore/LibraryCore.h>
#include <LibraryCore/ReexportedSubmodule.h>
#pragma once
struct LibraryType {};

//--- Library/Submodule.h
#include <LibraryCore/Submodule.h>
#pragma once
struct LibrarySubmoduleType {};

//--- LibraryClient.swift

import Library
import Library.Submodule

// PUBLIC-LABEL: public func foo
// PRIVATE-LABEL: public func foo
public func foo(
  // PUBLIC-SAME: a: Library::LibraryCoreType
  // PRIVATE-SAME: a: Library::LibraryCoreType
  a: LibraryCoreType,
  // PUBLIC-SAME: b: Library::LibraryCoreSubmoduleType
  // PRIVATE-SAME: b: LibraryCore::LibraryCoreSubmoduleType
  b: LibraryCoreSubmoduleType,
  // PUBLIC-SAME: c: Library::LibraryCoreReexportedSubmoduleType
  // PRIVATE-SAME: c: LibraryCore::LibraryCoreReexportedSubmoduleType
  c: LibraryCoreReexportedSubmoduleType,
  // PUBLIC-SAME: d: Library::LibraryType
  // PRIVATE-SAME: d: Library::LibraryType
  d: LibraryType,
  // PUBLIC-SAME: e: Library::LibrarySubmoduleType
  // PRIVATE-SAME: e: Library::LibrarySubmoduleType
  e: LibrarySubmoduleType
) {}

//--- LibraryCoreClient.swift

import LibraryCore
import LibraryCore.Submodule
import LibraryCore.ReexportedSubmodule

// PUBLIC-LABEL: public func foo
// PRIVATE-LABEL: public func foo
public func foo(
  // PUBLIC-SAME: a: Library::LibraryCoreType
  // PRIVATE-SAME: a: LibraryCore::LibraryCoreType
  a: LibraryCoreType,
  // PUBLIC-SAME: b: Library::LibraryCoreSubmoduleType
  // PRIVATE-SAME: b: LibraryCore::LibraryCoreSubmoduleType
  b: LibraryCoreSubmoduleType,
  // PUBLIC-SAME: c: Library::LibraryCoreReexportedSubmoduleType
  // PRIVATE-SAME: c: LibraryCore::LibraryCoreReexportedSubmoduleType
  c: LibraryCoreReexportedSubmoduleType
) {}
