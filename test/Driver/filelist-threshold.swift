// RUN: %empty-directory(%t)
// RUN: touch %t/a.swift %t/b.swift

// RUN: %swiftc_driver -driver-print-jobs -c -target x86_64-apple-macosx10.9 %t/a.swift %t/b.swift %S/../Inputs/empty.swift -module-name main -driver-filelist-threshold=99 2>&1 | %FileCheck -check-prefix=UNDERTHRESHOLD %s

// UNDERTHRESHOLD-NOT: -filelist
// UNDERTHRESHOLD-NOT: -primary-filelist
// UNDERTHRESHOLD-NOT: -supplementary-output-file-map
// UNDERTHRESHOLD-NOT: -output-filelist


// RUN: %swiftc_driver -driver-print-jobs -c -target x86_64-apple-macosx10.9 %t/a.swift %t/b.swift %S/../Inputs/empty.swift -module-name main -driver-filelist-threshold=0 | %FileCheck -check-prefix=OVERTHRESHOLD %s
// RUN: %swiftc_driver -driver-print-jobs -c -target x86_64-apple-macosx10.9 %t/a.swift %t/b.swift %S/../Inputs/empty.swift -module-name main -driver-use-filelists > %t/out.txt 2>&1
// RUN: %FileCheck -check-prefixes=OVERTHRESHOLD,DEPRECATED --input-file %t/out.txt %s

// DEPRECATED: warning: the option '-driver-use-filelists' is deprecated; use '-driver-filelist-threshold=0' instead
// OVERTHRESHOLD: -filelist
// OVERTHRESHOLD: -primary-filelist
// OVERTHRESHOLD: -supplementary-output-file-map
// OVERTHRESHOLD: -output-filelist
// OVERTHRESHOLD: -filelist
// OVERTHRESHOLD: -primary-filelist
// OVERTHRESHOLD: -supplementary-output-file-map
// OVERTHRESHOLD: -output-filelist
// OVERTHRESHOLD: -filelist
// OVERTHRESHOLD: -primary-filelist
// OVERTHRESHOLD: -supplementary-output-file-map
// OVERTHRESHOLD: -output-filelist


// RUN: %swiftc_driver -driver-print-jobs -c -target x86_64-apple-macosx10.9 %t/a.swift %t/b.swift %S/../Inputs/empty.swift -module-name main -driver-filelist-threshold=1 | %FileCheck -check-prefix=MIXEDTHRESHOLD %s

// MIXEDTHRESHOLD: -filelist
// MIXEDTHRESHOLD-NOT: -primary-filelist
// MIXEDTHRESHOLD: -supplementary-output-file-map
// MIXEDTHRESHOLD-NOT: -output-filelist
// MIXEDTHRESHOLD: -filelist
// MIXEDTHRESHOLD-NOT: -primary-filelist
// MIXEDTHRESHOLD: -supplementary-output-file-map
// MIXEDTHRESHOLD-NOT: -output-filelist
// MIXEDTHRESHOLD: -filelist
// MIXEDTHRESHOLD-NOT: -primary-filelist
// MIXEDTHRESHOLD: -supplementary-output-file-map
// MIXEDTHRESHOLD-NOT: -output-filelist
