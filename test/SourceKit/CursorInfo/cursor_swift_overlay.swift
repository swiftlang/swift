// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: sed -e "s@NAME_DIR@%{/t:regex_replacement}/A@g" -e "s@EXTERNAL_DIR@%{/t:regex_replacement}/B@g" %t/base.yaml > %t/overlay.yaml

//--- A/a.swift
func a() {
// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):3 %t/A/a.swift -- -vfsoverlay %t/overlay.yaml %t/A/a.swift %t/A/b.swift | %FileCheck %s
  b()
}

//--- B/b.swift
// TODO: This should be B/b.swift, but there's currently a bug with multiple overlays.
//       See rdar://90578880 or https://github.com/llvm/llvm-project/issues/53306.
//       Replace with CHECK-FIXED when that's fixed.
// CHECK: source.lang.swift.ref.function.free ({{.*}}{{/|\\}}A{{/|\\}}b.swift:*missing file*)
// CHECK-FIXED: source.lang.swift.ref.function.free ({{.*}}{{/|\\}}B{{/|\\}}b.swift:[[@LINE+1]]:6-[[@LINE+1]]:9)
func b() {}

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
