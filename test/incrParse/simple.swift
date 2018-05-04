// RUN: %empty-directory(%t)
// RUN: %incparse-test %s --test-case REPLACE
// RUN: %incparse-test %s --test-case REPLACE_BY_LONGER
// RUN: %incparse-test %s --test-case REPLACE_BY_SHORTER
// RUN: %incparse-test %s --test-case INSERT
// RUN: %incparse-test %s --test-case REMOVE

func foo() {
}

_ = <<REPLACE<6|||7>>>
_ = <<REPLACE_BY_LONGER<6|||"Hello World">>>
_ = <<REPLACE_BY_SHORTER<"Hello again"|||"a">>>
<<INSERT<|||foo()>>>
<<REMOVE<print("abc")|||>>>
_ = 1
