// RUN: not %target-swift-frontend %s -filelist nonexistent 2>&1 | %FileCheck -check-prefix=CHECK-BADFILEANDFILELIST %s
// CHECK-BADFILEANDFILELIST: error: cannot have input files with file list

// RUN: not %target-swift-frontend nonexistent.swift another-nonexistent.swift nonexistent.swift 2>&1 | %FileCheck -check-prefix=CHECK-APPEARSMORETHANONCE %s
// CHECK-APPEARSMORETHANONCE: error: duplicate input file 'nonexistent.swift'
