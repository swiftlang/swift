// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: rm -rf %t && mkdir -p %t 
// RUN: %target-build-swift %S/Inputs/capture_propagation_linkage/main.swift %s -O -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

// CHECK: test ok

// Capture propagation specializes the reabstraction thunk for createInstance
// and for main.createNil().
//
// We used to have a problem which caused us to mark these as private
// since both would produce the same name. Now we no longer have this
// problem due to the new mangler.

func createSome() -> MyClass? {
	return genericCaller(createInstance)
}

func createInstance() -> MyClass? {
    return MyClass()
}

