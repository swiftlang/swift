// RUN: %empty-directory(%t.mod)
// RUN: %swift -emit-module -o %t.mod/module_with_class_extension.swiftmodule %S/Inputs/module_with_class_extension.swift -parse-as-library
// RUN: %sourcekitd-test -req=doc-info -module module_with_class_extension -- -Xfrontend -disable-implicit-concurrency-module-import -I %t.mod > %t.response
// RUN: %diff -u %s.response %t.response

// rdar://76868074: Make sure we print the extensions for C.
