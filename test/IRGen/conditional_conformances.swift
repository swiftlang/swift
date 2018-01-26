// RUN: %target-swift-frontend -emit-ir %S/../Inputs/conditional_conformance_basic_conformances.swift | %FileCheck %S/../Inputs/conditional_conformance_basic_conformances.swift
// RUN: %target-swift-frontend -emit-ir %S/../Inputs/conditional_conformance_with_assoc.swift | %FileCheck %S/../Inputs/conditional_conformance_with_assoc.swift
// RUN: %target-swift-frontend -emit-ir %S/../Inputs/conditional_conformance_subclass.swift | %FileCheck %S/../Inputs/conditional_conformance_subclass.swift
// RUN: %target-swift-frontend -emit-ir %S/../Inputs/conditional_conformance_recursive.swift | %FileCheck %S/../Inputs/conditional_conformance_recursive.swift

// Too many pointer-sized integers in the IR
// REQUIRES: PTRSIZE=64

