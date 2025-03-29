//===--- FunctionUses.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Provides a list of instructions, which reference a function.
///
/// A function "use" is an instruction in another (or the same) function which
/// references the function. In most cases those are `function_ref` instructions,
/// but can also be e.g. `keypath` instructions.
///
/// 'FunctionUses' performs an analysis of all functions in the module and collects
/// instructions which reference other functions. This utility can be used to do
/// inter-procedural caller-analysis.
///
/// In order to use `FunctionUses`, first call `collect()` and then get use-lists of
/// functions with `getUses(of:)`.
struct FunctionUses {

  // Function uses are stored in a single linked list, whereas the "next" is not a pointer
  // but an index into `FunctionUses.useStorage`.
  fileprivate struct Use {
    // The index of the next use in `FunctionUses.useStorage`.
    let next: Int?

    // The instruction which references the function.
    let usingInstruction: Instruction
  }

  // The head of the single-linked list of function uses.
  fileprivate struct FirstUse {
    // The head of the use-list.
    var first: Int?
    
    // True if the function has unknown uses
    var hasUnknownUses: Bool

    init(of function: Function) {
      self.hasUnknownUses = function.isPossiblyUsedExternally || function.isDefinedExternally
    }

    mutating func insert(_ inst: Instruction, _ uses: inout [Use]) {
      let newFirst = uses.count
      uses.append(Use(next: first, usingInstruction: inst))
      first = newFirst
    }
  }

  /// The list of uses of a function.
  struct UseList : CollectionLikeSequence, CustomStringConvertible {
    struct Iterator : IteratorProtocol {
      fileprivate let useStorage: [Use]
      fileprivate var currentUseIdx: Int?
      
      mutating func next() -> Instruction? {
        if let useIdx = currentUseIdx {
          let use = useStorage[useIdx]
          currentUseIdx = use.next
          return use.usingInstruction
        }
        return nil
      }
    }

    // The "storage" for all function uses.
    fileprivate let useStorage: [Use]

    // The head of the single-linked use list.
    fileprivate let firstUse: FirstUse

    /// True if the function has unknown uses in addition to the list of referencing instructions.
    ///
    /// This is the case, e.g. if the function has public linkage or if the function
    /// is referenced from a vtable or witness table.
    var hasUnknownUses: Bool { firstUse.hasUnknownUses }

    func makeIterator() -> Iterator {
      return Iterator(useStorage: useStorage, currentUseIdx: firstUse.first)
    }
    
    var description: String {
      var result = "[\n"
      if hasUnknownUses {
        result += "<unknown uses>\n"
      }
      for inst in self {
        result += "@\(inst.parentFunction.name): \(inst)\n"
  
      }
      result += "]"
      return result
    }
    
    var customMirror: Mirror { Mirror(self, children: []) }
  }

  // The "storage" for all function uses.
  private var useStorage: [Use] = []

  // The use-list head for each function.
  private var uses: [Function: FirstUse] = [:]
  
  /// Returns the use-list of `function`.
  ///
  /// Note that `collect` must be called before `getUses` can be used.
  func getUses(of function: Function) -> UseList {
    UseList(useStorage: useStorage, firstUse: uses[function, default: FirstUse(of: function)])
  }

  /// Collects all uses of all function in the module.
  mutating func collect(context: ModulePassContext) {

    // Already start with a reasonable big capacity to reduce the number of
    // re-allocations when appending to the data structures.
    useStorage.reserveCapacity(128)
    uses.reserveCapacity(64)

    // Mark all functions, which are referenced from tables, to have "unknown" uses.

    for vTable in context.vTables {
      for entry in vTable.entries {
        markUnknown(entry.implementation)
      }
    }

    for witnessTable in context.witnessTables {
      for entry in witnessTable.entries {
        if case .method(_, let witness) = entry, let witness {
          markUnknown(witness)
        }
      }
    }

    for witnessTable in context.defaultWitnessTables {
      for entry in witnessTable.entries {
        if case .method(_, let witness) = entry, let witness {
          markUnknown(witness)
        }
      }
    }

    // Collect all instructions, which reference functions, in the module.
    for function in context.functions {
      for inst in function.instructions {
        inst.visitReferencedFunctions { referencedFunc in
          uses[referencedFunc, default: FirstUse(of: referencedFunc)].insert(inst, &useStorage)
        }
      }
    }
  }

  private mutating func markUnknown(_ function: Function) {
    uses[function, default: FirstUse(of: function)].hasUnknownUses = true
  }
}
