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

//--- Lib.swift
public func publicFunction() {}

//--- UseExisting.swift
import Lib

//--- Rebuild.swift
import Lib // expected-remark {{rebuilding module 'Lib' from interface}}
