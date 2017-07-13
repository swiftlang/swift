// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=interface-gen -are-swift-args -header %S/Inputs/header.h -- %s -enable-objc-interop -import-objc-header %S/Inputs/header.h > %t.response
// RUN: diff -u %S/gen_header.swift.response %t.response

doSomethingInHead(1)
