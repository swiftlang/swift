// RUN: not %target-swift-frontend -primary-filelist 2>&1 | %FileCheck -check-prefix=CHECK-EXPECTEDARGUMENT %s
// CHECK-EXPECTEDARGUMENT: error: missing argument value for '-primary-filelist', expected 1 argument(s)
// RUN: not %target-swift-frontend -primary-filelist nonexistant 2>&1 | %FileCheck -check-prefix=CHECK-BADFILE %s
// CHECK-BADFILE: error: cannot open file

// RUN: %empty-directory(%t)
// RUN: echo '%S/Inputs/filelist-other.swift' >> %t/input.txt
// RUN: echo '%s' >> %t/input.txt
// RUN: echo '%S/../Inputs/empty.swift' >> %t/input.txt
// RUN: echo '%s' >> %t/primary.txt
// RUN: not %target-swift-frontend -typecheck -filelist %t/input.txt -primary-filelist %t/primary.txt 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck '%S/Inputs/filelist-other.swift' '%s' -primary-filelist %t/primary.txt 2>&1 | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: echo '%S/Inputs/filelist-other.swift' >> %t/input.txt
// RUN: echo '%s' >> %t/input.txt
// RUN: echo '%S/../Inputs/empty.swift' >> %t/primary.txt
// RUN: echo '%s' >> %t/primary.txt
// RUN: not %target-swift-frontend -typecheck -filelist %t/input.txt -primary-filelist %t/primary.txt 2>&1 | %FileCheck -check-prefix=CHECK-PRIMARYNOTFOUND %s
// CHECK-PRIMARYNOTFOUND: error: primary file '{{.*}}/../Inputs/empty.swift' was not found in file list '{{.*}}/input.txt'

// RUN: not %target-swift-frontend -primary-file %s -primary-filelist nonexistent 2>&1 | %FileCheck -check-prefix=CHECK-BADFILEANDFILELIST %s
// CHECK-BADFILEANDFILELIST: error: cannot have primary input files with primary file list


func test() {
  #if !WORKING
    // Check with FileCheck because we want to see that this file is being
    // compiled.
    // CHECK: error: cannot convert value of type 'Bar' to specified type 'Foo'
    let x: Foo = other()
  #endif
}
