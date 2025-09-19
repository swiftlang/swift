// RUN: %empty-directory(%t)

// Stage in the headers we need
// RUN: mkdir %t/headers
// RUN: cp %S/../Inputs/c-bridging-header.h %t/headers

// RUN: mkdir %t/src
// RUN: split-file %s %t/src

// Build a module
// RUN: mkdir %t/modules
// RUN: %target-swift-frontend -internal-import-bridging-header %t/headers/c-bridging-header.h -sdk %clang-importer-sdk -emit-module -o %t/modules/MyModule.swiftmodule %t/src/MyModule.swift

// Check that there's no serialized bridging header in the module file.
// RUN: llvm-bcanalyzer -dump %t/modules/MyModule.swiftmodule | %FileCheck -check-prefix MODULE-FILE %s

// Use the module.
// RUN: %target-swift-frontend -typecheck -sdk %clang-importer-sdk -I %t/modules %t/src/MyClient.swift

// Delete the bridging header, and again use the module.
// RUN: rm %t/headers/c-bridging-header.h
// RUN: %target-swift-frontend -typecheck -sdk %clang-importer-sdk -I %t/modules %t/src/MyClient.swift

//--- MyModule.swift
func getRed() -> Color { red }

func getX(point: MyPoint) -> Double { point.x }

public func f() {
  _ = getRed()
}

//--- MyClient.swift

import MyModule

func g() {
  f()
}


// MODULE-FILE-NOT: IMPORTED_HEADER
// MODULE-FILE-NOT: IMPORTED_HEADER_CONTENTS

// MODULE-FILE: SEARCH_PATH

// MODULE-FILE-NOT: IMPORTED_HEADER
// MODULE-FILE-NOT: IMPORTED_HEADER_CONTENTS
