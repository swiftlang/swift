// RUN: %empty-directory(%t.mod)
// RUN: %swift -emit-module -o %t.mod/cake.swiftmodule %S/Inputs/cake.swift -parse-as-library -enable-objc-interop
// RUN: %sourcekitd-test -req=doc-info -module cake -- -I %t.mod > %t.response
// RUN: diff -u %s.response %t.response
