//===--- Check that we convert comments to XML correctly.

// RUN: %target-swift-frontend -parse -verify -disable-objc-attr-requires-foundation-module %S/../Inputs/comment_to_something_conversion.swift
// RUN: %target-swift-ide-test -print-comments -source-filename %S/../Inputs/comment_to_something_conversion.swift -comments-xml-schema %S/../../bindings/xml/comment-xml-schema.rng > %t.txt
// RUN: FileCheck %S/../Inputs/comment_to_something_conversion.swift < %t.txt
// RUN: FileCheck %s -check-prefix=WRONG < %t.txt

// WRONG-NOT: CommentXMLInvalid

