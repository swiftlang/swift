// RUN: rm -rf %t && mkdir -p %t 
// RUN: %target-build-swift %S/Inputs/capture_propagation_linkage/main.swift %s -O -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

// CHECK: test ok

// Capture propagation specializes the reabstraction thunk for createInstance
// and for main.createNil().
// Both specialized thunks have the same name and therefore they must be private.
// Otherwise the wrong thunk will be called.

func createSome() -> MyClass? {
	return genericCaller(createInstance)
}

func createInstance() -> MyClass? {
    return MyClass()
}

