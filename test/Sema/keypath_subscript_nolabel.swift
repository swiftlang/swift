// RUN: %target-swift-frontend -typecheck -verify -primary-file %s
// [SR-12745]
// rdar://problem/62957095
struct S1 {
  var x : Int = 0
}
var s1 = S1()
s1[\.x] = 10 // expected-error {{missing argument label 'keyPath:' in subscript}} {{4-4=keyPath: }}

struct S2 {
  var x : Int = 0
  subscript(_ v: Int) -> Int { 0 }
}
var s2 = S2()
s2[\.x] = 10  // expected-error {{missing argument label 'keyPath:' in subscript}} {{4-4=keyPath: }}

struct S3 {
  var x : Int = 0
  subscript(v v: KeyPath<S3, Int>) -> Int { get { 0 } set(newValue) {} }
}
var s3 = S3()
// TODO(diagnostics): This should actually be a diagnostic that correctly identifies that in the presence
// of a missing label, there are two options for resolution: 'keyPath' and 'v:' and to offer the user
// a choice.
// Today, the ExprTypeChecker identifies the disjunction with two of these possibilities, but 
// filters out some of the terms based on label mismatch (but not implicit keypath terms, for example).
// It should probably not do that.
s3[\.x] = 10 // expected-error {{missing argument label 'keyPath:' in subscript}} {{4-4=keyPath: }}

struct S4 {
  var x : Int = 0
  subscript(v: KeyPath<S4, String>) -> Int { get { 0 } set(newValue) {} }
}
var s4 = S4()
s4[\.x] = 10 // expected-error {{key path value type 'Int' cannot be converted to contextual type 'String'}}
