// RUN: %target-parse-verify-swift
//
// <rdar://problem/19422945> Parse/keyword_in_pattern_recovery.swift is XFAIL'ed and it regressed
// XFAIL: *
//
// Test recovery for the case when keywords are used as identifiers
// in patterns.

func keywordParamName1(self: Int) {}  // expected-error {{keyword 'self' cannot be used as an identifier}}
func keywordParamName2(Self: Int) {}  // expected-error {{keyword 'Self' cannot be used as an identifier}}
func keywordParamName3(in: Int) {}    // expected-error {{keyword 'in' cannot be used as an identifier}}
func keywordParamName4(func: Int) {}  // expected-error {{keyword 'func' cannot be used as an identifier}}
func keywordParamName5(class: Int) {} // expected-error {{keyword 'class' cannot be used as an identifier}}
func keywordParamName6(var: Int) {}   // expected-error {{expected pattern}}

var super = 123 // expected-error {{keyword 'super' cannot be used as an identifier}}

var self = 123 // expected-error {{keyword 'self' cannot be used as an identifier}}
var Self = 123 // expected-error {{keyword 'Self' cannot be used as an identifier}}

struct FooStruct {
  var super = 123 // expected-error {{keyword 'super' cannot be used as an identifier}}
  var self : Int // expected-error {{keyword 'self' cannot be used as an identifier}}
  var Self : Int // expected-error {{keyword 'Self' cannot be used as an identifier}}
}

