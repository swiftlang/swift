// RUN: %empty-directory(%t)
// RUN: %swift -emit-module -o %t %S/Inputs/cycle-depend/A.swift -I %S/Inputs/cycle-depend -enable-source-import
// RUN: %swift -emit-module -o %t %S/Inputs/cycle-depend/B.swift -I %S/Inputs/cycle-depend -enable-source-import

// RUN: %sourcekitd-test -req=index %t/A.swiftmodule -- %t/A.swiftmodule -I %t | %sed_clean > %t.response
// RUN: diff -u %S/Inputs/cycle-depend/A.response %t.response
