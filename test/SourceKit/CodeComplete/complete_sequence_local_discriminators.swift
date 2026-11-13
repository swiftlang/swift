func localType() {
  struct Local {}
  func take(_ value: Local) {}
  take()
}

func typeInClosure() {
  _ = {
    struct InClosure {}
    func take(_ value: InClosure) {}
    take()
  }
}

// The second completion reuses the function and reparses only its body. The
// new body's local types and closures still need discriminators, because
// completion mangles the expected type for the type relation of module results.

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=4:8 -repeat-request=2 %s -- %s -parse-as-library \
// RUN:   > %t.local.response
// RUN: %FileCheck --check-prefix=LOCAL %s < %t.local.response

// LOCAL-LABEL: key.results: [
// LOCAL: key.description: "(value: Local)"
// LOCAL-NOT: key.reusingastcontext: 1

// LOCAL-LABEL: key.results: [
// LOCAL: key.description: "(value: Local)"
// LOCAL: key.reusingastcontext: 1

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=11:10 -repeat-request=2 %s -- %s -parse-as-library \
// RUN:   > %t.closure.response
// RUN: %FileCheck --check-prefix=CLOSURE %s < %t.closure.response

// CLOSURE-LABEL: key.results: [
// CLOSURE: key.description: "(value: InClosure)"
// CLOSURE-NOT: key.reusingastcontext: 1

// CLOSURE-LABEL: key.results: [
// CLOSURE: key.description: "(value: InClosure)"
// CLOSURE: key.reusingastcontext: 1
