// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/UsesCxxStruct.swiftinterface) %s -I %S/Inputs -enable-library-evolution -swift-version 5 -enable-experimental-cxx-interop %S/Inputs/namespace-extension-lib.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/UsesCxxStruct.swiftinterface) -I %S/Inputs
// RUN: %FileCheck --input-file=%t/UsesCxxStruct.swiftinterface %s
// CHECK: -enable-experimental-cxx-interop

import Namespaces

var x: Namespace.Parent? = nil
