// Verifies that the caller generator handles C++ inheritance imported into
// Swift: C++ base-class members exposed on a derived foreign reference type get
// inherited-method callers, overridden members get `super` callers, and the
// generated file actually compiles against the imported module.

// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -print-module -module-to-print=Test -source-filename=x -I %t -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance > %t/Test-interface.swift
// RUN: %swift-function-caller-generator Test %t/Test-interface.swift > %t/out.swift
// RUN: %PathSanitizingDiff --sanitize-regex '=@available\(.*, \*\)\n' %t/out.expected < %t/out.swift
// RUN: %target-swift-frontend -typecheck %t/out.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance

//--- test.h
#define FRT __attribute__((swift_attr("import_reference"))) \
            __attribute__((swift_attr("retain:immortal")))  \
            __attribute__((swift_attr("release:immortal")))

struct FRT Base {
  int baseOnly() const { return 1; }
  virtual int shared() const { return 2; }
  int nonVirtualShared() const { return 20; }
};

struct Derived : Base {
  int shared() const override { return 3; }
  int nonVirtualShared() const { return 30; }
  int derivedOnly() const { return 4; }
};

struct LeafDerived : Derived {
  int shared() const override { return 5; }
};

//--- out.expected
import Test


extension Base {
  final func call_shared_Base() -> CInt {
    return shared()
  }
  final func call_baseOnly_Base() -> CInt {
    return baseOnly()
  }
  final func call_nonVirtualShared_Base() -> CInt {
    return nonVirtualShared()
  }
}

extension Derived {
  final func call_shared_Derived() -> CInt {
    return shared()
  }
  final func call_nonVirtualShared_Derived() -> CInt {
    return nonVirtualShared()
  }
  final func call_derivedOnly_Derived() -> CInt {
    return derivedOnly()
  }
  final func call_shared_Base_super() -> CInt {
    return super.shared()
  }
  final func call_baseOnly_Base_super() -> CInt {
    return super.baseOnly()
  }
  final func call_nonVirtualShared_Base_super() -> CInt {
    return super.nonVirtualShared()
  }
}

extension LeafDerived {
  final func call_shared_LeafDerived() -> CInt {
    return shared()
  }
  final func call_shared_Derived_super() -> CInt {
    return super.shared()
  }
  final func call_nonVirtualShared_Derived_super() -> CInt {
    return super.nonVirtualShared()
  }
  final func call_derivedOnly_Derived_super() -> CInt {
    return super.derivedOnly()
  }
}
//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}
