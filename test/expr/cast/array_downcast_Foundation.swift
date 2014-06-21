// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-swift-frontend %s -parse -verify
// REQUIRES: OS=macosx
// RUN: %target-swift-frontend %s -dump-ast -verify 2>&1 | FileCheck %s

import Foundation

func testDowncastObjectToArray(obj: AnyObject, objImplicit: AnyObject!) {
  var nsstrArr1 = (obj as NSString[])! // expected-error{{extraneous postfix '!'; forced downcast already produces a non-optional value of type 'NSString[]'}}{{38-39=}}
  var strArr1 = (obj as String[])! // expected-error{{extraneous postfix '!'; forced downcast already produces a non-optional value of type 'String[]'}}{{34-35=}}

  var nsstrArr2 = (objImplicit as NSString[])! // expected-error{{extraneous postfix '!'; forced downcast already produces a non-optional value of type 'NSString[]'}}{{46-47=}}
  var strArr2 = (objImplicit as String[])! // expected-error{{extraneous postfix '!'; forced downcast already produces a non-optional value of type 'String[]'}}{{42-43=}}
}

func testArrayDowncast(arr: AnyObject[], arrImplicit: AnyObject[]!) {
  var nsstrArr1 = (arr as NSString[])! // expected-error{{extraneous postfix '!'; forced downcast already produces a non-optional value of type 'NSString[]'}}
  var strArr1 = (arr as String[])! // expected-error{{extraneous postfix '!'; forced downcast already produces a non-optional value of type 'String[]'}}

  var nsstrArr2 = (arrImplicit as NSString[])! // expected-error{{extraneous postfix '!'; forced downcast already produces a non-optional value of type 'NSString[]'}}
  var strArr2 = (arrImplicit as String[])! // expected-error{{extraneous postfix '!'; forced downcast already produces a non-optional value of type 'String[]'}}
}

func testDowncastNSArrayToArray(nsarray: NSArray) {
  var nsstrArr1 = nsarray as NSString[]
  var strArr1 = nsarray as String[]
}

// CHECK-LABEL: testDowncastOptionalObject
func testDowncastOptionalObject(obj: AnyObject?!) -> String[]? {
  // CHECK: (optional_evaluation_expr implicit type='String[]?'
  // CHECK-NEXT: (inject_into_optional implicit type='String[]?'
  // CHECK-NEXT: (call_expr implicit type='String[]'
  // CHECK-NEXT: (dot_syntax_call_expr implicit type='(NSArray) -> Array<String>'
  // CHECK-NEXT: (declref_expr implicit type='(Array<String>.Type) -> (NSArray) -> Array<String>' decl=Foundation.(file).Array.bridgeFromObjectiveC
  // CHECK-NEXT: (type_expr implicit type='String[].Type' typerepr='<null>'))
  // CHECK-NEXT: (forced_checked_cast_expr type='NSArray'{{.*existential_to_concrete}}
  // CHECK-NEXT: (bind_optional_expr implicit type='AnyObject'
  // CHECK-NEXT: (force_value_expr implicit type='AnyObject?'
  // CHECK-NEXT: (declref_expr type='AnyObject?!' 
  return obj as String[]?
}

// CHECK-LABEL: testDowncastOptionalObjectConditional
func testDowncastOptionalObjectConditional(obj: AnyObject?!) -> String[]?? {
  // CHECK: (optional_evaluation_expr implicit type='String[]??'
  // CHECK-NEXT: (inject_into_optional implicit type='String[]??'
  // CHECK-NEXT: (optional_evaluation_expr implicit type='String[]?'
  // CHECK-NEXT: (inject_into_optional implicit type='String[]?'
  // CHECK-NEXT: (bind_optional_expr implicit type='String[]'
  // CHECK-NEXT: (optional_evaluation_expr implicit type='String[]?'
  // CHECK-NEXT: (call_expr implicit type='String[]?
  // CHECK-NEXT: (dot_syntax_call_expr implicit type='(NSArray) -> Array<String>?'
  // CHECK-NEXT: (declref_expr implicit type='(Array<String>.Type) -> (NSArray) -> Array<String>?'
  // CHECK-NEXT: (type_expr implicit type='String[].Type' typerepr='<null>'))
  // CHECK-NEXT: (bind_optional_expr implicit type='NSArray'
  // CHECK-NEXT: (conditional_checked_cast_expr type='NSArray?' {{.*existential_to_concrete}} writtenType=String[]?
  // CHECK-NEXT: (bind_optional_expr implicit type='AnyObject'
  // CHECK-NEXT: (bind_optional_expr implicit type='AnyObject?'
  // CHECK-NEXT: (declref_expr type='AnyObject?!'
  return obj as? String[]?
}

