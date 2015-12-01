// RUN: not --crash %target-swift-frontend %s -parse

// This test fails in the AST verifier, which can be turned off.
// REQUIRES: swift_ast_verifier

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class a<T where g:d{class A{class A<T>:A{init(){T{
