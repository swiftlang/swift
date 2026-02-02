// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk %s 2>&1 | %FileCheck %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk %s -enable-library-evolution 2>&1 | %FileCheck %s

// CHECK-NOT: internal bridging head

@available(*, deprecated)
func f() { }

func g(){
  f() // make sure we emit something
}
