// RUN: %empty-directory(%t)

// RUN: %target-build-swift %S/main.swift %S/Inputs/library.swift
// RUN: %target-build-swift -g %S/main.swift %S/Inputs/library.swift

func testFunction<T>(withCompletion completion: (Result<T, Error>) -> Void) { }
testFunction { (result: GenericResult<Int>) in }
