// XFAIL: linux
// RUN: rm -rf %t.mod
// RUN: mkdir -p %t.mod
// RUN: %swift -emit-module -o %t.mod/cake.swiftmodule %S/Inputs/cake.swift -parse-as-library
// RUN: %sourcekitd-test -req=doc-info -module cake -- -I %t.mod > %t.response
// RUN: diff -u %s.response %t.response
