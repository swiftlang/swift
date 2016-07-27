// RUN: rm -rf %t && mkdir %t

// RUN: %target-build-swift %S/main.swift %S/library.swift
// RUN: %target-build-swift -g %S/main.swift %S/library.swift

// REQUIRES: executable_test

func testFunction<T>(withCompletion completion: (Result<T, Error>) -> Void) { }
testFunction { (result: GenericResult<Int>) in }
