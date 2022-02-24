// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: sed -e "s@EXTERNAL_DIR@%{/t:regex_replacement}/A@g" -e "s@NAME_DIR@%{/t:regex_replacement}/B@g" %t/base.yaml > %t/overlay.yaml

//--- use.swift
import MyMod

// RUN: %sourcekitd-test -req=cursor -pos=%(line+3):13 %t/use.swift -- -I%t/B -vfsoverlay %t/overlay.yaml %t/use.swift | %FileCheck --check-prefix=CHECK-A %s
// RUN: %sourcekitd-test -req=cursor -pos=%(line+2):22 %t/use.swift -- -I%t/B -vfsoverlay %t/overlay.yaml %t/use.swift | %FileCheck --check-prefix=CHECK-B %s
// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):31 %t/use.swift -- -I%t/B -vfsoverlay %t/overlay.yaml %t/use.swift | %FileCheck --check-prefix=CHECK-C %s
func f(arg: A, arg2: B, arg3: C) {}

//--- A/A.h
// CHECK-A: source.lang.swift.ref.struct ({{.*}}{{/|\\}}A{{/|\\}}A.h:[[@LINE+1]]:8-[[@LINE+1]]:9)
struct A {
  int a;
};

//--- A/B.h

//--- A/C.h
// CHECK-C: source.lang.swift.ref.struct ({{.*}}{{/|\\}}A{{/|\\}}C.h:[[@LINE+1]]:8-[[@LINE+1]]:9)
struct C {
  int c;
};

//--- B/B.h
#include "C.h"

// CHECK-B: source.lang.swift.ref.struct ({{.*}}{{/|\\}}B{{/|\\}}B.h:[[@LINE+1]]:8-[[@LINE+1]]:9)
struct B {
  int b;
};

//--- B/module.modulemap
module MyMod {
  header "A.h"
  header "B.h"
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

