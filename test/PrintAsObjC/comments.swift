// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -enable-source-import -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc -module-name comments %S/../Inputs/comment_to_something_conversion.swift
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -parse-as-library %t/comments.swiftmodule -parse -emit-objc-header-path %t/comments.h
// RUN: sed -n -e '/A000/,$ p' %t/comments.h > %t/comments.h-cleaned
// RUN: diff %t/comments.h-cleaned %S/Inputs/comments-expected-output.h
// RUN: %check-in-clang %t/comments.h

