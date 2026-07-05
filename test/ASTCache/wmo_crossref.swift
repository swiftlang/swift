// RUN: rm -rf %t.cache %t
// RUN: mkdir -p %t.cache %t
// RUN: cp %s %t/a.swift
// RUN: echo 'struct Container { var items: [Item] }' > %t/b.swift
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -whole-module-optimization \
// RUN:   -typecheck %t/a.swift %t/b.swift 2>&1 | %FileCheck %s --check-prefix=COLD
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -whole-module-optimization \
// RUN:   -typecheck %t/a.swift %t/b.swift 2>&1 | %FileCheck %s --check-prefix=WARM

// Negative test: non-cached WMO build with a genuinely missing decl must
// still report the error (no false recovery from LoadedFromAstCache branch,
// which only fires when LoadedFromAstCache == true).
// RUN: printf 'struct Bad { var x: NonexistentType }\n' > %t/bad.swift
// RUN: not %target-swift-frontend -module-name badmod -parse-as-library -whole-module-optimization -typecheck %t/bad.swift 2>&1 | %FileCheck %s --check-prefix=NEGATIVE

// NEGATIVE: error: cannot find type
// COLD-DAG: AST cache: MISS (no cache file) for {{.*}}a.swift
// COLD-DAG: AST cache: MISS (no cache file) for {{.*}}b.swift
// COLD-DAG: AST cache: SAVED for {{.*}}a.swift
// COLD-DAG: AST cache: SAVED for {{.*}}b.swift

// WARM-DAG: AST cache: HIT for {{.*}}a.swift
// WARM-DAG: AST cache: HIT for {{.*}}b.swift


// Test BUG-1 fix: WMO cross-reference crash. When a cached .swiftast file
// has cross-references to same-module types, WMO type-checking may produce
// different DeclKind/type signatures than per-file, causing DeclKindChanged
// errors in resolveCrossReference(). The LoadedFromAstCache recovery branch
// returns the pre-filter match instead of crashing.

// a.swift defines Item, which is referenced by Container in b.swift.
// WMO compilation sees both files together, so cross-references are resolved
// against the full module context.

struct Item {
  var id: Int
  var name: String

  func displayName() -> String {
    return "\(name) (#\(id))"
  }
}

func describe(_ item: Item) -> String {
  return item.displayName()
}
