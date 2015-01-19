// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/has_generic_witness.swift
// RUN: llvm-bcanalyzer %t/has_generic_witness.swiftmodule | FileCheck %s
// RUN: %target-swift-frontend -emit-ir -I %t %s -o /dev/null

// We have to perform IRGen to actually check that the generic substitutions
// are being used.

// CHECK-NOT: UnknownCode

import has_generic_witness

var cfoo : Fooable = FooClass()
var sfoo : Fooable = FooStruct()

func bar<B: Barrable>(b: B) {}
bar(BarClass())
bar(BarStruct())

var cbas : Bassable = BasClass()
var sbas : Bassable = BasStruct()

func cyc<C: CyclicAssociatedType>(c: C) {}
cyc(CyclicImpl())
