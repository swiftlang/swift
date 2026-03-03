// RUN: not %target-swift-frontend -enable-upcoming-feature ImportObjcForwardDeclarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes %s -diagnostic-style llvm 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -swift-version 6 -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes %s -diagnostic-style llvm 2>&1 | %FileCheck  %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

import IncompleteTypeLibrary1
import IncompleteNoRootTypeProtocolLibrary

let incompleteInterface = CFunctionReturningAForwardDeclaredInterface1()!
let incompleteProtocol = CFunctionReturningAForwardDeclaredProtocol1()!
let incompleteNoRootTypeProtocol = CFunctionReturningAForwardDeclaredNoRootTypeProtocol()!

incompleteInterface.doSomethingForwardDeclaredInterfacesCan()
// CHECK: incomplete_objc_types_importing_limitations_diagnostics.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'ForwardDeclaredInterface' has no member 'doSomethingForwardDeclaredInterfacesCan'
// CHECK: incompleteInterface.doSomethingForwardDeclaredInterfacesCan()
// CHECK: incomplete-type-library-1.h:{{[0-9]+}}:{{[0-9]+}}: note: class 'ForwardDeclaredInterface' will be imported as an opaque placeholder class and may be missing members; import the definition to access the complete interface
// CHECK: @class ForwardDeclaredInterface;
// CHECK: ^
// CHECK: incomplete-type-library-1.h:{{[0-9]+}}:{{[0-9]+}}: note: interface 'ForwardDeclaredInterface' forward declared here
// CHECK: @class ForwardDeclaredInterface;
// CHECK: ^

incompleteProtocol.doSomethingForwardDeclaredProtocolsCan()
// CHECK: incomplete_objc_types_importing_limitations_diagnostics.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'any ForwardDeclaredProtocol & NSObjectProtocol' has no member 'doSomethingForwardDeclaredProtocolsCan'
// CHECK: incompleteProtocol.doSomethingForwardDeclaredProtocolsCan()
// CHECK: incomplete-type-library-1.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ForwardDeclaredProtocol' will be imported as an opaque placeholder protocol and may be missing members; import the definition to access the complete protocol
// CHECK: @protocol ForwardDeclaredProtocol;
// CHECK: ^
// CHECK: incomplete-type-library-1.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'ForwardDeclaredProtocol' forward declared here
// CHECK: @protocol ForwardDeclaredProtocol;
// CHECK: ^

incompleteNoRootTypeProtocol.doSomethingForwardDeclaredProtocolsCan()
// CHECK: incomplete_objc_types_importing_limitations_diagnostics.swift:{{[0-9]+}}:{{[0-9]+}}: error: value of type 'any NoRootTypeProtocol' has no member 'doSomethingForwardDeclaredProtocolsCan'
// CHECK: incompleteNoRootTypeProtocol.doSomethingForwardDeclaredProtocolsCan()
// CHECK: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// CHECK: incomplete-noroottype-protocol-library.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'NoRootTypeProtocol' will be imported as an opaque placeholder protocol and may be missing members; import the definition to access the complete protocol
// CHECK: @protocol NoRootTypeProtocol;
// CHECK: ^
// CHECK: incomplete-noroottype-protocol-library.h:{{[0-9]+}}:{{[0-9]+}}: note: protocol 'NoRootTypeProtocol' forward declared here
// CHECK: @protocol NoRootTypeProtocol;
// CHECK: ^

