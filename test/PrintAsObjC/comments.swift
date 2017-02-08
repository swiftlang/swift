// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-source-import -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc -module-name comments %S/../Inputs/comment_to_something_conversion.swift -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/comments.swiftmodule -typecheck -emit-objc-header-path %t/comments.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: awk '/A000/,0' %t/comments.h > %t/comments.h-cleaned
// RUN: diff %t/comments.h-cleaned %S/Inputs/comments-expected-output.h
// RUN: %check-in-clang -Wno-documentation %t/comments.h

// REQUIRES: objc_interop
// REQUIRES: no_asan
