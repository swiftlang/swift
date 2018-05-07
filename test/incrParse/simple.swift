// RUN: %empty-directory(%t)
// RUN: %incparse-test %s --test-case REPLACE
// RUN: %incparse-test %s --test-case REPLACE_BY_LONGER
// RUN: %incparse-test %s --test-case REPLACE_BY_SHORTER
// RUN: %incparse-test %s --test-case INSERT
// RUN: %incparse-test %s --test-case REMOVE
// RUN: %incparse-test %s --test-case ATTACH_TO_PREV_NODE
// RUN: %incparse-test %s --test-case CLASS_SURROUNDING
// RUN: %incparse-test %s --test-case MULTI_EDIT

func foo() {
}

_ = <<REPLACE<6|||7>>>
_ = <<REPLACE_BY_LONGER<6|||"Hello World">>>
_ = <<REPLACE_BY_SHORTER<"Hello again"|||"a">>>
<<INSERT<|||foo()>>>
<<REMOVE<print("abc")|||>>>
foo()
<<ATTACH_TO_PREV_NODE<|||{}>>>
_ = 1

<<CLASS_SURROUNDING<|||class C {>>>
  func method1() {}

<<MULTI_EDIT<|||class C {>>>
  func method1() {}
<<MULTI_EDIT<|||}>>>

