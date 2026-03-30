// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/HiddenDep.swift \
// RUN:   -o %t/HiddenDep.swiftmodule -module-name HiddenDep \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -disable-implicit-swift-modules -parse-stdlib

// RUN: %target-swift-frontend -emit-module %t/TestedLib.swift \
// RUN:   -o %t/TestedLib.swiftmodule -module-name TestedLib \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -disable-implicit-swift-modules -parse-stdlib \
// RUN:   -swift-module-file=HiddenDep=%t/HiddenDep.swiftmodule \
// RUN:   -enable-testing

// RUN: rm %t/HiddenDep.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Client.swift \
// RUN:   -swift-version 5 -disable-implicit-swift-modules -parse-stdlib \
// RUN:   -swift-module-file=TestedLib=%t/TestedLib.swiftmodule 

//--- HiddenDep.swift

public struct HiddenType {}

//--- TestedLib.swift

@_implementationOnly import HiddenDep

internal func internalFunc() {}

//--- Client.swift

@testable import TestedLib

internalFunc()
