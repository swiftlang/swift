// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -primary-file %s %S/Inputs/init_accessor_multifile_other.swift -module-name InitAccMulti
// RUN: %target-swift-emit-silgen -primary-file %s %S/Inputs/init_accessor_multifile_other.swift -module-name InitAccMulti | %FileCheck %s

// Constructor in this file implicitly refers to Holder's stored properties declared in
// the other file. Triggers a case where the type of a property may not be resolved
// during SILGen in emitMemberInitializationViaInitAccessor.

extension Holder {
  public init(action f: () -> Void) {}
}

// CHECK-LABEL: sil [ossa] @$s12InitAccMulti6HolderV6actionACyyXE_tcfC :
