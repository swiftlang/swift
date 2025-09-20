// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk %s 2>&1 | %FileCheck -check-prefix NONRESILIENT %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk %s -enable-library-evolution 2>&1 | %FileCheck -check-prefix EVOLUTION %s

// NONRESILIENT: warning: using internal bridging headers without library evolution can cause instability

// EVOLUTION-NOT: internal bridging head

@available(*, deprecated)
func f() { }

func g(){
  f() // make sure we emit something
}
