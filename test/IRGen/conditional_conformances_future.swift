// RUN: %target-swift-frontend %use_no_opaque_pointers -prespecialize-generic-metadata -target %module-target-future -emit-ir %S/../Inputs/conditional_conformance_basic_conformances.swift | %FileCheck %S/../Inputs/conditional_conformance_basic_conformances_future.swift -DINT=i%target-ptrsize --check-prefix=CHECK --check-prefix=%target-os
// RUN: %target-swift-frontend %use_no_opaque_pointers -prespecialize-generic-metadata -target %module-target-future -emit-ir %S/../Inputs/conditional_conformance_with_assoc.swift | %FileCheck %S/../Inputs/conditional_conformance_with_assoc_future.swift -DINT=i%target-ptrsize --check-prefix=CHECK --check-prefix=CHECK-STABLE-ABI-%target-mandates-stable-abi
// RUN: %target-swift-frontend %use_no_opaque_pointers -prespecialize-generic-metadata -target %module-target-future -emit-ir %S/../Inputs/conditional_conformance_subclass.swift | %FileCheck %S/../Inputs/conditional_conformance_subclass_future.swift -DINT=i%target-ptrsize --check-prefix=CHECK --check-prefix=CHECK-STABLE-ABI-%target-mandates-stable-abi
// RUN: %target-swift-frontend %use_no_opaque_pointers -prespecialize-generic-metadata -target %module-target-future -emit-ir %S/../Inputs/conditional_conformance_recursive.swift | %FileCheck %S/../Inputs/conditional_conformance_recursive.swift -DINT=i%target-ptrsize --check-prefix=CHECK --check-prefix=CHECK-STABLE-ABI-%target-mandates-stable-abi

// RUN: %target-swift-frontend -prespecialize-generic-metadata -target %module-target-future -emit-ir %S/../Inputs/conditional_conformance_basic_conformances.swift
// RUN: %target-swift-frontend -prespecialize-generic-metadata -target %module-target-future -emit-ir %S/../Inputs/conditional_conformance_with_assoc.swift
// RUN: %target-swift-frontend -prespecialize-generic-metadata -target %module-target-future -emit-ir %S/../Inputs/conditional_conformance_subclass.swift
// RUN: %target-swift-frontend -prespecialize-generic-metadata -target %module-target-future -emit-ir %S/../Inputs/conditional_conformance_recursive.swift

// Too many pointer-sized integers in the IR
// REQUIRES: PTRSIZE=64
// REQUIRES: VENDOR=apple || OS=linux-gnu

