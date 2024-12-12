// RUN: %empty-directory(%t)

// Check if fragile Swift interface with struct
// extensions can be reparsed:
// RUN: %target-swift-frontend -swift-version 5 -typecheck -emit-module-interface-path %t/UsesCxxStruct.swiftinterface %s -I %S/Inputs -swift-version 5 -enable-experimental-cxx-interop %S/Inputs/namespace-extension-lib.swift
// RUN: %target-swift-frontend -swift-version 5 -typecheck-module-from-interface  %t/UsesCxxStruct.swiftinterface -I %S/Inputs -enable-experimental-cxx-interop
// RUN: %FileCheck --input-file=%t/UsesCxxStruct.swiftinterface %s

// The textual module interface should not contain the C++ interop flag.
// CHECK-NOT: -enable-experimental-cxx-interop
// CHECK-NOT: -cxx-interoperability-mode


// Check if resilient Swift interface with builtin
// type extensions can be reparsed:
// RUN: %target-swift-emit-module-interface(%t/ResilientStruct.swiftinterface) %s -I %S/Inputs -enable-library-evolution -swift-version 5 -enable-experimental-cxx-interop %S/Inputs/namespace-extension-lib.swift -DRESILIENT
// RUN: %target-swift-typecheck-module-from-interface(%t/ResilientStruct.swiftinterface) -I %S/Inputs -DRESILIENT -enable-experimental-cxx-interop
// RUN: %FileCheck --input-file=%t/ResilientStruct.swiftinterface %s

import Namespaces

var x: Namespace.Parent? = nil
