// RUN: %target-swift-frontend -typecheck -Rinferred-types-at 9:12 -verify %s

func bar<T>() -> T? { nil /* No remarks should be emitted for this literal */ }

func foo(x: Int) -> String? {
  let myBool = (1 + x == 2) ? false : true // No remarks should be emitted for this expression
  return if myBool { // expected-remark {{statement expression was inferred to be of type 'String?'}}
  // expected-remark@-1 {{declaration reference was inferred to be of type 'Bool'}}
    "hello, world!" // expected-remark {{string literal was inferred to be of type 'String'}}
    // expected-remark@-1 {{optional injection expression was inferred to be of type 'String?'}}
  } else {
    bar() // expected-remark {{call expression was inferred to be of type 'String?'}}
    // expected-remark@-1 {{declaration reference was inferred to be of type '() -> String?'}}
  }
}