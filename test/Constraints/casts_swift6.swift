// RUN: %target-typecheck-verify-swift -enable-objc-interop -swift-version 6

func id<T>(_ x: T) -> T { x }
func ohno<T>(_ x: T) -> T? { nil }

// Swift 6 version of the test in casts.swift
func test_compatibility_coercions(_ arr: [Int], _ optArr: [Int]?, _ dict: [String: Int], _ set: Set<Int>, _ i: Int, _ stringAnyDict: [String: Any]) {
  // These have always been fine.
  _ = arr as [Any]?
  _ = dict as [String: Int]?
  _ = set as Set<Int>

  // These have always been errors.
  _ = arr as [String] // expected-error {{cannot convert value of type '[Int]' to type '[String]' in coercion}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('Int' and 'String') are expected to be equal}}
  _ = dict as [String: String] // expected-error {{cannot convert value of type '[String : Int]' to type '[String : String]' in coercion}}
  // expected-note@-1 {{arguments to generic parameter 'Value' ('Int' and 'String') are expected to be equal}}
  _ = dict as [String: String]? // expected-error {{'[String : Int]' is not convertible to '[String : String]?'}}
  // expected-note@-1 {{did you mean to use 'as!' to force downcast?}} {{12-14=as!}}
  _ = (dict as [String: Int]?) as [String: Int] // expected-error {{value of optional type '[String : Int]?' must be unwrapped to a value of type '[String : Int]'}}
  // expected-note@-1 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
  // expected-note@-2 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
  _ = set as Set<String>  // expected-error {{cannot convert value of type 'Set<Int>' to type 'Set<String>' in coercion}}
   // expected-note@-1 {{arguments to generic parameter 'Element' ('Int' and 'String') are expected to be equal}}

  // Make sure we error on the following in Swift 6 mode.
  _ = id(arr) as [String] // expected-error {{conflicting arguments to generic parameter 'T' ('[Int]' vs. '[String]')}}
  _ = (arr ?? []) as [String] // expected-error {{conflicting arguments to generic parameter 'T' ('[Int]' vs. '[String]')}}
  _ = (arr ?? [] ?? []) as [String] // expected-error {{conflicting arguments to generic parameter 'T' ('[Int]' vs. '[String]')}}
  // expected-error@-1{{conflicting arguments to generic parameter 'T' ('[Int]' vs. '[String]')}}
  _ = (optArr ?? []) as [String] // expected-error {{conflicting arguments to generic parameter 'T' ('[Int]' vs. '[String]'}}

  _ = (arr ?? []) as [String]? // expected-error {{'[Int]' is not convertible to '[String]?'}}
  // expected-note@-1 {{did you mean to use 'as!' to force downcast?}}
  _ = (arr ?? []) as [String?]? // expected-error {{'[Int]' is not convertible to '[String?]?'}}
  // expected-note@-1 {{did you mean to use 'as!' to force downcast?}}
  _ = (arr ?? []) as [String??]?? // expected-error {{'[Int]' is not convertible to '[String??]??'}}
  // expected-note@-1 {{did you mean to use 'as!' to force downcast?}}
  _ = (dict ?? [:]) as [String: String?]? // expected-error {{'[String : Int]' is not convertible to '[String : String?]?'}}
  // expected-note@-1 {{did you mean to use 'as!' to force downcast?}}
  _ = (set ?? []) as Set<String>?? // expected-error {{'Set<Int>' is not convertible to 'Set<String>??'}}
  // expected-note@-1 {{did you mean to use 'as!' to force downcast?}}

  _ = ohno(ohno(ohno(arr))) as [String] // expected-error {{cannot convert value of type '[Int]???' to type '[String]' in coercion}}
  _ = ohno(ohno(ohno(arr))) as [Int] // expected-error {{cannot convert value of type '[Int]???' to type '[Int]' in coercion}}
  _ = ohno(ohno(ohno(Set<Int>()))) as Set<String> // expected-error {{cannot convert value of type 'Set<Int>???' to type 'Set<String>' in coercion}}
  _ = ohno(ohno(ohno(["": ""]))) as [Int: String] // expected-error {{cannot convert value of type '[String : String]???' to type '[Int : String]' in coercion}}
  _ = ohno(ohno(ohno(dict))) as [String: Int] // expected-error {{cannot convert value of type '[String : Int]???' to type '[String : Int]' in coercion}}

  // In this case the array literal can be inferred to be [String], so totally
  // valid.
  _ = ([] ?? []) as [String] // expected-warning {{left side of nil coalescing operator '??' has non-optional type '[String]', so the right side is never used}}
  _ = (([] as Optional) ?? []) as [String]

  // The array can also be inferred to be [Any].
  _ = ([] ?? []) as Array // expected-warning {{left side of nil coalescing operator '??' has non-optional type '[Any]', so the right side is never used}}

  // Cases from rdar://88334481
  typealias Magic<T> = T
  _ = [i] as [String] // expected-error {{cannot convert value of type 'Int' to expected element type 'String'}}
  _ = [i] as Magic as [String] // expected-error {{cannot convert value of type 'Int' to expected element type 'String'}}
  _ = ([i]) as Magic as [String] // expected-error {{cannot convert value of type 'Int' to expected element type 'String'}}
  _  = [i: i] as [String: Any] // expected-error {{cannot convert value of type 'Int' to expected dictionary key type 'String'}}
  _  = ([i: i]) as [String: Any] // expected-error {{cannot convert value of type 'Int' to expected dictionary key type 'String'}}
  _  = [i: stringAnyDict] as [String: Any] // expected-error {{cannot convert value of type 'Int' to expected dictionary key type 'String'}}

  _ = [i].self as Magic as [String] // expected-error {{cannot convert value of type 'Int' to expected element type 'String'}}
  _ = (try [i]) as Magic as [String] // expected-error {{cannot convert value of type 'Int' to expected element type 'String'}}
  // expected-warning@-1 {{no calls to throwing functions occur within 'try' expression}}

  // These are wrong, but make sure we don't warn about the value cast always succeeding.
  _  = [i: i] as! [String: Any]
  _  = [i: stringAnyDict] as! [String: Any]
}
