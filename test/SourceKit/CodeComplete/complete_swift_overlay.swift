// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: sed -e "s@NAME_DIR@%{/t:regex_replacement}/A@g" -e "s@EXTERNAL_DIR@%{/t:regex_replacement}/B@g" %t/base.yaml > %t/overlay.yaml

// RUN: %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_check_dependency_interval=0 == \
// RUN:   -req=complete -pos=2:5 %t/A/a.swift -- -vfsoverlay %t/overlay.yaml %t/A/a.swift %t/A/b.swift == \
// RUN:   -req=complete -pos=2:5 %t/A/a.swift -- -vfsoverlay %t/overlay.yaml %t/A/a.swift %t/A/b.swift \
// RUN:   | %FileCheck %s

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.name: "method()"
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.name: "method()"
// CHECK: ]
// CHECK: key.reusingastcontext: 1


//--- A/a.swift
func a(b: B) {
  b.
}

//--- B/b.swift
struct B {
  func method() {}
}

//--- base.yaml
{
  version: 0,
  redirecting-with: "fallback",
  use-external-names: true,
  roots: [
    {
      type: "directory-remap",
      name: "NAME_DIR",
      external-contents: "EXTERNAL_DIR"
    }
  ]
}
