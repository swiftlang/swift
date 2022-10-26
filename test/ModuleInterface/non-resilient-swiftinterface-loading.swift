// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/NonResilientLib.swift \
// RUN:   -module-name NonResilientLib -swift-version 5 \
// RUN:   -emit-module-path %t/NonResilientLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/NonResilientLib.swiftinterface 2>&1 \
// RUN:   | grep "warning: module interfaces are only supported with -enable-library-evolution"

// RUN: %target-swift-frontend -emit-module %t/ResilientLib.swift \
// RUN:   -module-name ResilientLib -swift-version 5 \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/ResilientLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/ResilientLib.swiftinterface

/// Expecting no diagnostics when importing the swiftmodule.
// RUN: %target-swift-frontend -typecheck %t/ClientClean.swift \
// RUN:   -swift-version 5 -I %t -verify

/// Expecting diagnostics when importing the non-resilient swiftinterface.
// RUN: rm %t/*.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/ClientError.swift \
// RUN:   -swift-version 5 -I %t -verify

// RUN: env SWIFT_ACCEPT_NON_RESILIENT_INTERFACES=1 \
// RUN:   %target-swift-frontend -typecheck %t/ClientWarning.swift \
// RUN:   -swift-version 5 -I %t -verify

//--- NonResilientLib.swift

public func NonResilientFunc() {}

//--- ResilientLib.swift

public func ResilientFunc() {}

//--- ClientClean.swift

import NonResilientLib
import ResilientLib

NonResilientFunc()
ResilientFunc()

//--- ClientError.swift

import NonResilientLib // expected-error {{module 'NonResilientLib' was rebuilt from a swiftinterface without library evolution; it cannot be used to build a binary}}
import ResilientLib

NonResilientFunc()
ResilientFunc()

//--- ClientWarning.swift

import NonResilientLib // expected-warning {{module 'NonResilientLib' was rebuilt from a swiftinterface without library evolution; it cannot be used to build a binary}}
import ResilientLib

NonResilientFunc()
ResilientFunc()
