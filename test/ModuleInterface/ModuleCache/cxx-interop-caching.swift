// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Build one library module
// RUN: %target-swift-frontend -parse-stdlib -emit-module \
// RUN:   -o %t/Lib.swiftmodule %t/Lib.swift \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -swift-version 5 -enable-library-evolution -module-name Lib

/// Distributed scenario with only the swiftinterface
// RUN: rm %t/Lib.swiftmodule
// RUN: %empty-directory(%t/ModuleCache)

/// Rebuild from the swiftinterface just once for non-C++ users
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/Rebuild.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify

/// Rebuild from the swiftinterface just once for C++ users
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/Rebuild.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -cxx-interoperability-mode=default


/// Local library-evolution scenario where the swiftmodule is available
// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -parse-stdlib -emit-module \
// RUN:   -o %t/Lib.swiftmodule %t/Lib.swift \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -swift-version 5 -enable-library-evolution -module-name Lib

/// Don't rebuild for non-C++ interop users.
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify

/// Rebuild from swiftinterface for C++ interop users. (Disabled)
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -cxx-interoperability-mode=default

/// Rebuild from swiftinterface for C++ interop users.
// RUN: env SWIFT_ENABLE_SWIFTMODULE_PER_CXX_INTEROP=1 \
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/RebuildLocal.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -cxx-interoperability-mode=default -verify-ignore-unknown


/// Test local non-libary-evolution module scenario, library without C++ interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -parse-stdlib -emit-module \
// RUN:   -o %t/Lib.swiftmodule %t/Lib.swift \
// RUN:   -swift-version 5 -module-name Lib

/// When matching, don't rebuild even when check is enabled
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify
// RUN: env SWIFT_ENABLE_SWIFTMODULE_PER_CXX_INTEROP=1 \
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify

/// On mismatch, rebuild only when check is enabled
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -cxx-interoperability-mode=default
// RUN: env SWIFT_ENABLE_SWIFTMODULE_PER_CXX_INTEROP=1 \
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/LocalError_NonCxxLib.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -verify-ignore-unknown -show-diagnostics-after-fatal


/// Test local non-libary-evolution module scenario, library with C++ interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -parse-stdlib -emit-module \
// RUN:   -o %t/Lib.swiftmodule %t/Lib.swift \
// RUN:   -swift-version 5 -module-name Lib \
// RUN:   -cxx-interoperability-mode=default

/// When matching, don't rebuild even when check is enabled
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -cxx-interoperability-mode=default
// RUN: env SWIFT_ENABLE_SWIFTMODULE_PER_CXX_INTEROP=1 \
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/UseExisting.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -cxx-interoperability-mode=default -verify-ignore-unknown

/// On mismatch, rebuild only when check is enabled
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/LocalError_CxxLib_Legacy.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify

// RUN: env SWIFT_ENABLE_SWIFTMODULE_PER_CXX_INTEROP=1 \
// RUN: %target-swift-frontend -parse-stdlib -typecheck %t/LocalError_CxxLib.swift -I %t \
// RUN:   -module-cache-path %t/ModuleCache -Rmodule-interface-rebuild -verify \
// RUN:   -verify-ignore-unknown -show-diagnostics-after-fatal

//--- Lib.swift
public func publicFunction() {}

//--- UseExisting.swift
import Lib

//--- Rebuild.swift
import Lib // expected-remark {{rebuilding module 'Lib' from interface}}

//--- RebuildLocal.swift
import Lib // expected-remark {{rebuilding module 'Lib' from interface}}
// expected-note @-1 {{compiled module is out of date}}
// expected-note @-2 {{compiled with a different C++ interop mode}}

//--- LocalError_NonCxxLib.swift
import Lib // expected-error {{cannot load module 'Lib' built with the C++ interop disabled from a module where it's enabled}}

//--- LocalError_CxxLib.swift
import Lib // expected-error {{cannot load module 'Lib' built with the C++ interop enabled from a module where it's disabled}}

//--- LocalError_CxxLib_Legacy.swift
import Lib // expected-error {{module 'Lib' was built with C++ interoperability enabled, but current compilation does not enable C++ interoperability}}
// expected-note @-1 {{visit https://www.swift.org/documentation/cxx-interop/project-build-setup to learn how to enable C++ interoperability}}
