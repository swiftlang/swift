//===--- ConformanceCheckOptimization.swift -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

/// Optimizes protocol conformance checking by pre-populating vtables with conformance information
/// for "fast-cast" protocols that have superclass constraints.
///
/// This optimization works by:
/// 1. Identifying classes that are eligible for optimization (have fixed metadata layout,
///    are not open access, and belong to the current module)
/// 2. Finding protocols, enabled for fast casting nad that have superclass
///    constraints and belong to the current module
/// 3. Pre-computing conformance checks for these protocols and storing the results directly
///    in the vtable, eliminating the need for runtime conformance lookups
///
/// The optimization improves performance by avoiding expensive runtime protocol conformance
/// checks for protocols that are eligible for fast casting and have well-defined inheritance
/// hierarchies within the current module.
let conformanceCheckOptimization = ModulePass(name: "conformance-check-optimization") {
  (moduleContext: ModulePassContext) in

  guard // We need to see the whole class hierarchy of the module.
        moduleContext.isWholeModule,
        // Casts to existentials are currently not supported in Embedded Swift. This would need some
        // more work in IRGen.
        !moduleContext.options.enableEmbeddedSwift
  else {
    return
  }

  // Map of classes to all protcols which have this class as a superclass constraint.
  let protocolsBySuperClass = getProtocolsBySuperClass(moduleContext)

  if protocolsBySuperClass.isEmpty {
    // Avoid going through all the vtables again if there is nothing to do.
    return
  }

  for vTable in moduleContext.vTables {
    let classType = vTable.class.declaredInterfaceType

    // Add all conformance entries for this class. This needs to be done in order of
    // base- to derived class so that base-class entries have consistent indices in all
    // vtables of the whole class hierarchy.
    for c in Array(vTable.class.selfAndSuperClasses).reversed() {
      for proto in protocolsBySuperClass[c, default: []] {
        let conf = classType.checkConformance(to: proto)
        if conf.isValid {
          vTable.append(conformance: .conformance(conf))
        } else {
          vTable.append(conformance: .noConformance(proto))
        }
      }
    }
  }
}

/// Builds a mapping of class declarations to protocols that have those classes as superclass constraints.
///
/// Only considers protocols which are valid for fast conformance casting.
private func getProtocolsBySuperClass(_ moduleContext: ModulePassContext) -> Dictionary<ClassDecl, [ProtocolDecl]> {
  let notEligibleClasses = getNotEligibleClasses(moduleContext)

  var addedProtocols = Set<ProtocolDecl>()
  var superClassesOfProtocols = Dictionary<ClassDecl, [ProtocolDecl]>()
  let currentModule = moduleContext.moduleDecl

  for vTable in moduleContext.vTables {
    let classDecl = vTable.class
    for proto in classDecl.allProtocols  {
      if // "fast casting" implies that there must not be any retroactive conformances in other modules.
         proto.isEligibleForFastCasting,

         // We can only reason about conformances if the protocol and its conforming classes are in the
         // current module. Otherwise there might be conformances in other modules.
         proto.parentModule == currentModule,

         let protoSuperClass = proto.anySuperClassDecl,
         !notEligibleClasses.contains(protoSuperClass)
      {
        assert(classDecl.selfAndSuperClasses.contains(protoSuperClass))
        if addedProtocols.insert(proto).inserted {
          superClassesOfProtocols[protoSuperClass, default: []].append(proto)
        }
      }
    }
  }
  return superClassesOfProtocols
}

private func getNotEligibleClasses(_ moduleContext: ModulePassContext) -> Set<ClassDecl> {
  var notEligibleClasses = Set<ClassDecl>()
  let currentModule = moduleContext.moduleDecl

  for vTable in moduleContext.vTables {
    let classDecl = vTable.class
    if // IRGen can only add conformance entries to a class metadata if the metadata is computed
       // at compile time. This is e.g. not the case for generic class for which the metadata is
       // created at runtime.
       !classDecl.hasFixedMetadataLayout(moduleContext) ||

       // We can only reason about conformances if the protocol and its conforming classes are in the
       // current module. Otherwise there might be conformances in other modules.
       classDecl.hasOpenAccess(in: nil) ||
       classDecl.parentModule != currentModule
    {
      // If a class is not eligible, all its super classes are also not eligible. If conformance entries
      // are added for a class, they must be present for all its derived classes as well.
      for superCl in classDecl.selfAndSuperClasses {
        guard notEligibleClasses.insert(superCl).inserted else {
          break
        }
      }
    }
  }
  return notEligibleClasses
}

private extension ProtocolDecl {
  /// Returns the first superclass constraint found for this protocol by looking at this
  /// and inherited protocols.
  ///
  /// If there are multiple superclass constraints (i.e. if the protocol inherits from multiple
  /// other superclass constraint protocols) it's fine that we pick the first one.
  /// It's a very uncommon case and the optimization works fine if we don't consider all superclass
  /// constraints.
  var anySuperClassDecl: ClassDecl? {
    if let superClass = superClassDecl {
      return superClass
    }
    for baseProto in allInheritedProtocols {
      if let baseSuperClass = baseProto.superClassDecl {
        return baseSuperClass
      }
    }
    return nil
  }
}
