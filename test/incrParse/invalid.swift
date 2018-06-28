// RUN: %empty-directory(%t)
// RUN: %incparse-test %s --test-case NO_CHANGES
// RUN: %incparse-test %s --test-case NESTED_INITIALIZERS

func start() {}

class Bar

let y = 1

class NestedInitializers {
  <<NESTED_INITIALIZERS<|||init() {>>>
    init() {

    }
  <<NESTED_INITIALIZERS<|||}>>>
}
