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
/// eventually folded into the `export_as` module itself. Library isn't loaded,
/// so the private interface refers to LibraryCore.
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

/// Build modules that import Library, with and without re-exporting it.
// RUN: %target-swift-frontend -emit-module %t/Dep.swift \
// RUN:   -module-name Dep -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Dep.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/ReexportingDep.swift \
// RUN:   -module-name ReexportingDep -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/ReexportingDep.swiftmodule

/// Build a client of the LibraryCore lib that imports Library only internally.
/// Library is loaded, but the private interface doesn't import it, so the
/// interface must refer to LibraryCore.
// RUN: %target-swift-frontend -emit-module %t/LibraryCoreInternalImportClient.swift \
// RUN:   -module-name LibraryCoreInternalImportClient -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/LibraryCoreInternalImportClient.swiftmodule \
// RUN:   -emit-private-module-interface-path %t/LibraryCoreInternalImportClient.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/LibraryCoreInternalImportClient.private.swiftinterface) -module-name LibraryCoreInternalImportClient -I %t
// RUN: %FileCheck -input-file=%t/LibraryCoreInternalImportClient.private.swiftinterface -check-prefix=PRIVATE %t/LibraryCoreInternalImportClient.swift

/// Build a client of the LibraryCore lib that imports a module that imports
/// Library without re-exporting it. Library is loaded, but clients of the
/// private interface can't refer to it, so the interface must refer to
/// LibraryCore.
// RUN: %target-swift-frontend -emit-module %t/LibraryCoreDepClient.swift \
// RUN:   -module-name LibraryCoreDepClient -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/LibraryCoreDepClient.swiftmodule \
// RUN:   -emit-private-module-interface-path %t/LibraryCoreDepClient.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/LibraryCoreDepClient.private.swiftinterface) -module-name LibraryCoreDepClient -I %t
// RUN: %FileCheck -input-file=%t/LibraryCoreDepClient.private.swiftinterface -check-prefix=PRIVATE %t/LibraryCoreDepClient.swift

/// Build a client of the LibraryCore lib that imports a module that re-exports
/// Library. Clients of the private interface can refer to Library through that
/// module, so the interface refers to Library.
// RUN: %target-swift-frontend -emit-module %t/LibraryCoreReexportingDepClient.swift \
// RUN:   -module-name LibraryCoreReexportingDepClient -swift-version 5 -I %t \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/LibraryCoreReexportingDepClient.swiftmodule \
// RUN:   -emit-private-module-interface-path %t/LibraryCoreReexportingDepClient.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/LibraryCoreReexportingDepClient.private.swiftinterface) -module-name LibraryCoreReexportingDepClient -I %t
// RUN: %FileCheck -input-file=%t/LibraryCoreReexportingDepClient.private.swiftinterface -check-prefix=PRIVATE %t/LibraryCoreReexportingDepClient.swift

//--- module.modulemap
module LibraryCore {
  export_as Library

  umbrella "LibraryCore"
  header "LibraryCore/LibraryCore.h"
  export *

  module NonExplicitSubmodule {
    header "LibraryCore/NonExplicitSubmodule.h"
    export *
  }

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
#include "LibraryCore/NonExplicitSubmodule.h"
struct LibraryCoreType {};

//--- LibraryCore/Submodule.h
// This file is re-exported by Library.Submodule (which Swift does not respect)
#pragma once
struct LibraryCoreSubmoduleType {};

//--- LibraryCore/NonExplicitSubmodule.h
// This file is re-exported by Library. Its submodule isn't explicit, so it's
// visible whenever LibraryCore is.
#pragma once
struct LibraryCoreNonExplicitSubmoduleType {};

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
  // PUBLIC-SAME: c2: Library::LibraryCoreNonExplicitSubmoduleType
  // PRIVATE-SAME: c2: Library::LibraryCoreNonExplicitSubmoduleType
  c2: LibraryCoreNonExplicitSubmoduleType,
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
  c: LibraryCoreReexportedSubmoduleType,
  // PUBLIC-SAME: c2: Library::LibraryCoreNonExplicitSubmoduleType
  // PRIVATE-SAME: c2: LibraryCore::LibraryCoreNonExplicitSubmoduleType
  c2: LibraryCoreNonExplicitSubmoduleType
) {}

//--- Dep.swift

import Library

public func dep() {}

//--- ReexportingDep.swift

@_exported import Library

public func reexportingDep() {}

//--- LibraryCoreInternalImportClient.swift

import LibraryCore
internal import Library

// PRIVATE-LABEL: public func foo
public func foo(
  // PRIVATE-SAME: a: LibraryCore::LibraryCoreType
  a: LibraryCoreType,
  // PRIVATE-SAME: c2: LibraryCore::LibraryCoreNonExplicitSubmoduleType
  c2: LibraryCoreNonExplicitSubmoduleType
) {}

//--- LibraryCoreDepClient.swift

import LibraryCore
import Dep

// PRIVATE-LABEL: public func foo
public func foo(
  // PRIVATE-SAME: a: LibraryCore::LibraryCoreType
  a: LibraryCoreType,
  // PRIVATE-SAME: c2: LibraryCore::LibraryCoreNonExplicitSubmoduleType
  c2: LibraryCoreNonExplicitSubmoduleType
) {}

//--- LibraryCoreReexportingDepClient.swift

import LibraryCore
import ReexportingDep

// PRIVATE-LABEL: public func foo
public func foo(
  // PRIVATE-SAME: a: Library::LibraryCoreType
  a: LibraryCoreType,
  // PRIVATE-SAME: c2: Library::LibraryCoreNonExplicitSubmoduleType
  c2: LibraryCoreNonExplicitSubmoduleType
) {}
