// RUN: %empty-directory(%t)
// RUN: %incparse-test %s --test-case ADD_PROPERTY
// RUN: %incparse-test %s --test-case WRAP_IN_CLASS
// RUN: %incparse-test %s --test-case UNWRAP_CLASS

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


<reparse WRAP_IN_CLASS><<WRAP_IN_CLASS<|||class Foo {>>></reparse WRAP_IN_CLASS>
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