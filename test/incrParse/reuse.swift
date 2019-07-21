// RUN: %empty-directory(%t)
// RUN: %validate-incrparse %s --test-case ADD_PROPERTY
// RUN: %validate-incrparse %s --test-case WRAP_IN_CLASS
// RUN: %validate-incrparse %s --test-case UNWRAP_CLASS
// RUN: %validate-incrparse %s --test-case NEXT_TOKEN_CALCULATION

func start() {}

<reparse ADD_PROPERTY>struct Foo {</reparse ADD_PROPERTY>
  let a: Int
  let b: Int
  let c: Int
<reparse ADD_PROPERTY>
  let d: String
  <<ADD_PROPERTY<|||let e_newProp: String>>>
  let f: Int
</reparse ADD_PROPERTY>
  let g: Int
<reparse ADD_PROPERTY>}</reparse ADD_PROPERTY>

// FIXME: The functions inside the class should not need to get reparsed
<<WRAP_IN_CLASS<|||class Foo {>>>
  func foo1() {
    print("Hello Foo!")
  }

  func foo2() {
    print("Hello again")
  }

<<UNWRAP_CLASS<class Bar {|||>>>
  func bar1() {
    let pi = 3.1415
    print("Pi is (approximately) \(pi)")
  }

  func bar2() {
    print("I can compute Pi as well:")
    bar1()
  }
}

// The indentation on these lines is important for the test case
    let a = "hello"
<reparse NEXT_TOKEN_CALCULATION>    let c = "<<NEXT_TOKEN_CALCULATION< |||>>>world"</reparse NEXT_TOKEN_CALCULATION>

