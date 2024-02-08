//===--- ManglingUtils.swift --------------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

/// Represents a parse tree for a demangled string.
/// 
/// The type is not directly constructible and can only be obtained 
/// by demangling a mangled string using the `Demangler`. The lifetime of
/// such a `DemangledNode` ends with the lifetime of the `Demangler`.
struct DemangledNode {
    private let bridged: BridgedNode

    fileprivate init(_ bridged: BridgedNode) {
        self.bridged = bridged
    }

    public var kind: DemangledNodeKind {
        bridged.getKind()
    }

    public var firstChild: DemangledNode? {
        guard let firstChild = bridged.getFirstChild().value else { return nil }
        return DemangledNode(firstChild)
    }

    public var numChildren: UInt {
        bridged.getNumChildren()
    }

    public var index: UInt {
        bridged.getIndex()
    }

    public var text: StringRef {
        StringRef(bridged: bridged.getText())
    }

    public func child(at index: UInt) -> DemangledNode? {
        guard let child = bridged.getChild(index).value else { return nil }
        return DemangledNode(child)
    }
}

/// The demangler.
///
/// It de-mangles a string and it also owns the returned 
/// `DemangledNode`. This means the nodes of the tree only
///  live as long as the Demangler itself.
/// 
/// Users must call `deinitialize` to destroy the demangler
/// and all its nodes, when done using it.
struct Demangler {
    public var bridged: BridgedDemangler

    init() {
        bridged = BridgedDemangler()
    }

    init(bridged: BridgedDemangler) {
        self.bridged = bridged
    }

    public func deinitialize() {
        bridged.destroy()
    }

    public func demangleSymbol(mangledName: String) -> DemangledNode? {
        guard let node = mangledName._withBridgedStringRef({ bridged.demangleSymbol($0) }).value else { return nil }
        return DemangledNode(node)
    }

    public func providePreallocatedMemory(parent: inout Demangler) {
        bridged.providePreallocatedMemory(&parent.bridged)
    }
}

/// A demangler which uses stack space for its initial memory.
/// Currently the demangler always using a stack space of 1024 
/// bytes.
struct StackAllocatedDemangler {
    public var bridged: BridgedStackAllocatedDemangler

    init() {
        bridged = BridgedStackAllocatedDemangler()
    }

    public func asDemangler() -> Demangler {
        return Demangler(bridged: bridged.asBridgedDemangler())
    }
}

/// The mangler for functions where arguments or effects are specialized.
struct FunctionSignatureSpecializationMangler {
    private var bridged: BridgedFunctionSignatureSpecializationMangler

    init(specializationPass: SpecializationPass, isSerialized: Bool, function: Function) {
        bridged = BridgedFunctionSignatureSpecializationMangler(specializationPass, isSerialized, function.bridged)
    }

    public func setArgumentClosureProp(argIndex: UInt, instruction: BridgedInstruction) {
        bridged.setArgumentClosureProp(argIndex, instruction)
    }

    public func mangle() -> String {
        return String(taking: bridged.mangle())
    }    
}

// Bridging utilities

extension OptionalBridgedNode {
    public var value: BridgedNode? {
        if let node = node {
            return BridgedNode(node)
        } 
        return nil
    }
}
