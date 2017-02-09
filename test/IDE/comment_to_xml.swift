//===--- Check that we convert comments to XML correctly.

// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -typecheck -verify -disable-objc-attr-requires-foundation-module %S/../Inputs/comment_to_something_conversion.swift
// RUN: %target-swift-ide-test -new-mangling-for-tests -print-comments -source-filename %S/../Inputs/comment_to_something_conversion.swift -comments-xml-schema %S/../../bindings/xml/comment-xml-schema.rng > %t.txt
// RUN: %FileCheck %S/../Inputs/comment_to_something_conversion.swift < %t.txt
// RUN: %FileCheck %s -check-prefix=WRONG < %t.txt

// REQUIRES: no_asan
// WRONG-NOT: CommentXMLInvalid
