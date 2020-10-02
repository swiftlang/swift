// REQUIRES: OS=maccatalyst

// RUN: %empty-directory(%t.mod)
// RUN: %swift -emit-module -target x86_64-apple-ios13.1-macabi -o %t.mod/availability.swiftmodule %S/Inputs/availability_maccatalyst_is_deprecated.swift -parse-as-library -emit-module-doc-path %t.mod/availability.swiftdoc
// RUN: %sourcekitd-test -req=doc-info -module availability -- -target x86_64-apple-ios13.1-macabi -I %t.mod > %t.response
// RUN: %diff -u %s.response %t.response
