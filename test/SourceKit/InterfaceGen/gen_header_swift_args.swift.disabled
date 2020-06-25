// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=interface-gen -using-swift-args -header %S/Inputs/header.h -- %s -Xfrontend -enable-objc-interop -import-objc-header %S/Inputs/header.h > %t.response
// RUN: %diff -u %S/gen_header.swift.response %t.response

doSomethingInHead(1)
