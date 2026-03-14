// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// UNSUPPORTED: CPU=i386

// RUN: %empty-directory(%t)

// RUN: echo "public func foo() -> some CustomStringConvertible { 32 }" > %t/source1.swift
// RUN: echo "" > %t/source2.swift

// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -module-name multifile %t/source1.swift  %t/source2.swift -Xfrontend -validate-tbd-against-ir=all -emit-tbd -emit-tbd-path %t/multifile.tbd -Xfrontend -tbd-install_name -Xfrontend multifile -enable-library-evolution -emit-module

// RUN: %validate-json %t/multifile.tbd | %FileCheck %s 

// CHECK: s9multifile3fooQryFQOMQ
