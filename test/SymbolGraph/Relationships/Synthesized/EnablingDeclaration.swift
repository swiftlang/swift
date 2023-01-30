// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/RemoteP.swift -module-name RemoteP -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name EnablingDeclaration -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name EnablingDeclaration -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/EnablingDeclaration@RemoteP.symbols.json --check-prefix=SYNTH 
// RUN: %FileCheck %s --input-file %t/EnablingDeclaration@RemoteP.symbols.json --check-prefix=NOSYNTH
import RemoteP

// unrelated to the P protocol and P.extraFunc()
public extension PImpl {
  func foo() {}
}

// NOSYNTH-NOT: {{"PImpl",[[:space:]]*"extraFunc\(\)"}}

// related to the P protocol and enables the synthesized member NoImpl.extraFunc()
extension NoPImpl: P {
    public func someFunc() {}

    public func otherFunc() {}

    public func bonusFunc() {}
}

// SYNTH: {{"NoPImpl",[[:space:]]*"extraFunc\(\)"}}
