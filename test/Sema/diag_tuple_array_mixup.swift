// RUN: %target-typecheck-verify-swift

@available(anyAppleOS 26, *)
func testArrayMismatchDiagnostics() -> (Int, Int, Int, Int) {
  takeInlineArray((1, 2, 3, 4))                          // expected-error {{array of type '[4 of Int]' cannot be used with tuple}} expected-note {{did you mean to use an array literal instead?}} {{19-20=[}} {{30-31=]}}
  takeArray((1, 2, 3, 4))                                // expected-error {{array of type '[Int]' cannot be used with tuple}} expected-note {{did you mean to use an array literal instead?}} {{13-14=[}} {{24-25=]}}
  takeTuple([1, 2, 3, 4])                                // expected-error {{tuple of type '(Int, Int, Int, Int)' cannot be used with array literal}} expected-note {{did you mean to use a tuple instead?}} {{13-14=(}} {{24-25=)}}

  takeInlineArrayThatCouldBeMistakenForSize2((1, 2))    // expected-error {{cannot convert value of type '(Int, Int)' to expected argument type 'InlineArray<4294967298, Int>'}} {{none}}

  var inlineArray: [4 of Int] = (1, 2, 3, 4)             // expected-error {{array of type '[4 of Int]' cannot be initialized with tuple}} expected-note {{did you mean to use an array literal instead?}} {{33-34=[}} {{44-45=]}}
  var array: [Int] = (1, 2, 3, 4)                        // expected-error {{array of type '[Int]' cannot be initialized with tuple}} expected-note {{did you mean to use an array literal instead?}} {{22-23=[}} {{33-34=]}}
  var tuple: (Int, Int, Int, Int) = [1, 2, 3, 4]         // expected-error {{tuple of type '(Int, Int, Int, Int)' cannot be initialized with array literal}} expected-note {{did you mean to use a tuple instead?}} {{37-38=(}} {{48-49=)}}

  inlineArray = (1, 2, 3, 4)                             // expected-error {{array of type '[4 of Int]' cannot be used with tuple}} expected-note {{did you mean to use an array literal instead?}} {{17-18=[}} {{28-29=]}}
  array = (1, 2, 3, 4)                                   // expected-error {{array of type '[Int]' cannot be used with tuple}} expected-note {{did you mean to use an array literal instead?}} {{11-12=[}} {{22-23=]}}
  tuple = [1, 2, 3, 4]                                   // expected-error {{tuple of type '(Int, Int, Int, Int)' cannot be used with array literal}} expected-note {{did you mean to use a tuple instead?}} {{11-12=(}} {{22-23=)}}

  _ = inlineArray.0                                      // expected-error {{cannot access element using tuple member for non-tuple type '[4 of Int]'; did you mean to use '[0]'?}} {{18-20=[0]}}
  _ = inlineArray.99                                     // expected-error {{value of type '[4 of Int]' has no member '99'}} {{none}}
  _ = array.0                                            // expected-error {{cannot access element using tuple member for non-tuple type '[Int]'; did you mean to use '[0]'?}} {{12-14=[0]}}
  _ = tuple.0

  _ = inlineArray[0]
  _ = array[0]
  _ = tuple[0]                                           // expected-error {{cannot access element using subscript for tuple type '(Int, Int, Int, Int)'; did you mean to use '.0'?}} {{12-15=.0}}
  _ = tuple[99]                                          // expected-error {{cannot access element using subscript for tuple type '(Int, Int, Int, Int)'; use '.' notation instead}} {{none}}

  return [1, 2, 3, 4]                                    // expected-error {{tuple of type '(Int, Int, Int, Int)' cannot be used with array literal}} expected-note {{did you mean to use a tuple instead?}} {{10-11=(}} {{21-22=)}}
}

@available(anyAppleOS 26, *)
func takeInlineArray(_: [4 of Int]) {}
func takeArray(_: [Int]) {}
func takeTuple(_: (Int, Int, Int, Int)) {}

// Size is 2^32 + 2
@available(anyAppleOS 26, *)
func takeInlineArrayThatCouldBeMistakenForSize2(
  _: InlineArray<4294967298, Int>
) {}
