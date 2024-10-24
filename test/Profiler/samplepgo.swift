// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Test use-sample-profile attribute is present only when SampleFDO
// is enabled. A bogus profile is fine here.

// RUN: %target-swift-frontend %t/program.swift -module-name test -emit-ir -o %t/no-attr.ll

// RUN: %target-swift-frontend %t/program.swift -profile-sample-use %t/profile.txt \
// RUN:               %t/program.swift -module-name test -emit-ir %s -o %t/has-attr.ll
//
// RUN: %FileCheck %s < %t/has-attr.ll

// CHECK: anything


//--- program.swift
public func anything() {}


//--- profile.txt
bar:100:100
 1: 2000
