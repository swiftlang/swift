//===--- Check that we convert comments to XML correctly.

// RUN: %swift -parse -verify %S/../Inputs/comment_to_something_conversion.swift
// RUN: %swift-ide-test -print-comments -source-filename %S/../Inputs/comment_to_something_conversion.swift -comments-xml-schema %S/../../bindings/xml/comment-xml-schema.rng > %t.txt
// RUN: FileCheck %S/../Inputs/comment_to_something_conversion.swift < %t.txt
// RUN: FileCheck %s -check-prefix=WRONG < %t.txt

// WRONG-NOT: CommentXMLInvalid

