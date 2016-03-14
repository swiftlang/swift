// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// This test fails in the AST verifier, which can be turned off.
// REQUIRES: swift_ast_verifier
class a<T where g:d{class A{class A<T>:A{init(){T{
