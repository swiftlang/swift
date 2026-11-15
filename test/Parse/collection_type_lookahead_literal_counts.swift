// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -parse -verify -enable-experimental-feature LiteralExpressions %s
// RUN: %target-swift-frontend -parse -verify -enable-experimental-feature LiteralExpressions %S/nested_collection_type_lookahead.swift

// Parenthesized literal counts still require their special lookahead. A
// parenthesized tuple or function type instead follows the ordinary type path.
struct Box<T> {}
func expressionCounts(_: [(1 + 2) of Int], _: [(1 + 2) of [3 of Int]]) {}
let _ = Box<[(1 + 2) of Int]>.self
let _ = Box<[(Int, String)]>.self
let _ = Box<[(() -> Int)]>.self

// A postfix or function-type suffix after the parentheses does not form a
// parenthesized literal count. Preserve ordinary expression recovery.
let _ = Box<[(Int)? of Int]>.self // expected-error {{expected ',' separator}}
let _ = Box<[() -> Int of Int]>.self // expected-error {{expected ',' separator}}
