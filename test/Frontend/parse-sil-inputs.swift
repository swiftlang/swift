// RUN: not %target-swift-frontend -parse-sil -emit-sil 2>&1 | %FileCheck -check-prefix=SIL_FILES %s
// SIL_FILES: this mode requires a single input file

// RUN: not %target-swift-frontend -parse-sil -emit-sil %/s %/s 2>&1 | %FileCheck -check-prefix=DUPLICATE_FILES %s
// RUN: not %target-swift-frontend -parse-sil -emit-sil %/s %/S/../Inputs/empty.swift 2>&1 | %FileCheck -check-prefix=SIL_FILES %s
// DUPLICATE_FILES: duplicate input file 'SOURCE_DIR/test/Frontend/parse-sil-inputs.swift'
