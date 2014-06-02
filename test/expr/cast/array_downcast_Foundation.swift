// RUN: rm -rf %t  &&  mkdir %t
// RUN: %target-swift-frontend %s -parse -verify
// REQUIRES: OS=macosx

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
