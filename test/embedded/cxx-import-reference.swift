// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -I %t %t/Main.swift -enable-experimental-feature Embedded -cxx-interoperability-mode=default -c -o %t/a.o -Rmodule-loading

// REQUIRES: swift_feature_Embedded

// BEGIN header.h

class C;

void retainC(C * _Nonnull obj);
void releaseC(C * _Nonnull obj);

class C {
public:
  C(const C &) = delete;
  C() {};
  
  virtual void foo();

  static C * _Nonnull create () __attribute__((swift_attr("returns_retained")));
}
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainC")))
__attribute__((swift_attr("release:releaseC")));

// BEGIN module.modulemap

module MyModule {
  header "header.h"
}

// BEGIN Main.swift

import MyModule

public func test()
{
  let c =  C.create()
  c.foo()
}
