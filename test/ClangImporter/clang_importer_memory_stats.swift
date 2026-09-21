// Verify the Clang/Swift memory statistics emitted via -stats-output-dir
// (aggregate counters + per-module clang-module-memory.json) and the
// -print-clang-stats stderr dump.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/stats)

// --- Aggregate counters in the stats JSON.
// RUN: %target-swift-frontend -typecheck -cxx-interoperability-mode=default -I %t/Inputs %t/test.swift -module-name Test -stats-output-dir %t/stats
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv %t/stats
// RUN: %FileCheck --check-prefix=CSV -input-file %t/frontend.csv %s

// CSV-DAG: {{"AST.SwiftASTContextBytes".*[1-9][0-9]*$}}
// CSV-DAG: {{"ClangImporter.ClangASTContextBytes".*[1-9][0-9]*$}}
// CSV-DAG: {{"ClangImporter.ClangTotalBytes".*[1-9][0-9]*$}}
// CSV-DAG: {{"ClangImporter.NumLoadedClangModules".*[1-9][0-9]*$}}
// CSV-DAG: {{"ClangImporter.NumMaterializedClangDecls".*[1-9][0-9]*$}}

// --- Per-module breakdown JSON file (name carries a pid suffix).
// RUN: cat %t/stats/clang-module-memory-*.json | %FileCheck --check-prefix=PERMOD %s

// PERMOD: "numLoadedModules": {{[1-9][0-9]*}}
// PERMOD: "modules": [
// PERMOD-DAG: "module": "CxxMod"
// PERMOD-DAG: "materializedDecls": {{[1-9][0-9]*}}

// --- Human-readable stderr dump via -print-clang-stats.
// RUN: %target-swift-frontend -typecheck -cxx-interoperability-mode=default -I %t/Inputs %t/test.swift -module-name Test -print-clang-stats 2>&1 | %FileCheck --check-prefix=STDERR %s

// STDERR: *** Clang importer memory ***
// STDERR: Clang ASTContext bytes
// STDERR: Per-module (sorted by materialized decls)
// STDERR: CxxMod

//--- Inputs/module.modulemap
module CxxMod {
  header "cxxmod.h"
  requires cplusplus
  export *
}

//--- Inputs/cxxmod.h
#pragma once
struct Point {
  int x;
  int y;
};

//--- test.swift
import CxxMod

func use() {
  var p = Point(x: 1, y: 2)
  p.x = 3
  _ = p.y
}
