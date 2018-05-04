// RUN: %empty-directory(%t)
// RUN: %incparse-test %s --test-case REPLACE

func foo() {
}

_ = <<REPLACE<6|||7>>>
_ = 1
