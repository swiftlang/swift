// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -o %t/main %S/Inputs/ForeignTypeMetadata1.swift  %S/Inputs/ForeignTypeMetadata2.swift %t/main.swift -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main


// REQUIRES: executable_test
// REQUIRES: objc_interop

useType()
