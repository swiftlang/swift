// A test that ensures SWIFT_PRIVATE_FILEID annotations on nested C++ classes
// works as expected.
//
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/file1.swift -I %t/include -cxx-interoperability-mode=default -module-name main
// RUN: %target-swift-frontend -typecheck -verify %t/file2.swift -I %t/include -cxx-interoperability-mode=default -module-name main

//--- include/module.modulemap
module CxxModule {
    requires cplusplus
    header "cxx-header.h"
}

//--- include/cxx-header.h
#ifndef CXX_HEADER_H
#define CXX_HEADER_H

class __attribute__((__swift_attr__("private_fileid:main/file1.swift"))) OK {
private:
  void priv(void) const {}
};

class __attribute__((__swift_attr__("private_fileid:main/file1.swift"))) Outer {
public:
  class __attribute__((__swift_attr__("private_fileid:main/file2.swift"))) Inner {
  private:
    void priv(void) const {}
  };
private:
  void priv(void) const {}
};
#endif /* CXX_HEADER_H */

//--- file1.swift
import CxxModule
extension OK {
    func ext() {
        priv()
    }
}
extension Outer {
    func ext() {
        priv()
    }
}
extension Outer.Inner {
    func ext() {
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}

//--- file2.swift
import CxxModule
extension OK {
    func ext() {
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
extension Outer {
    func ext() {
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
extension Outer.Inner {
    func ext() {
        priv()
    }
}
