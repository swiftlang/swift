// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple

// -fvisibility=hidden hides every unannotated declaration.
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -Xcc -fvisibility=hidden -verify-additional-prefix fvis-hidden-

// REQUIRES: swift_feature_CxxImplementation
// UNSUPPORTED: OS=windows-msvc

//--- module.modulemap
module Vis {
  header "Vis.h"
  requires cplusplus
}

//--- Vis.h
__attribute__((visibility("hidden"))) int hiddenPublic();
int defaultPublic();

namespace __attribute__((visibility("hidden"))) HiddenNS {
int inHiddenNamespace();
}

struct __attribute__((visibility("hidden"))) HiddenStruct {
  int value;
  int get() const;
};

struct HiddenRef;
void retainHiddenRef(HiddenRef *_Nonnull);
void releaseHiddenRef(HiddenRef *_Nonnull);

struct __attribute__((visibility("hidden")))
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainHiddenRef")))
__attribute__((swift_attr("release:releaseHiddenRef"))) HiddenRef {
  int value;
  virtual int describe() const;
};

//--- main.swift
import Vis

@cxx @implementation
public func hiddenPublic() -> Int32 { 0 }
// expected-warning@-1 {{global function 'hiddenPublic()' is declared 'public' but will not be exported, because C++ function 'hiddenPublic' has hidden visibility}}{{1-8=}}

@cxx @implementation
public func defaultPublic() -> Int32 { 0 }
// expected-fvis-hidden-warning@-1 {{global function 'defaultPublic()' is declared 'public' but will not be exported, because C++ function 'defaultPublic' has hidden visibility}}{{1-8=}}

extension HiddenNS {
  @cxx @implementation
  public static func inHiddenNamespace() -> Int32 { 0 }
  // expected-warning@-1 {{static method 'inHiddenNamespace()' is declared 'public' but will not be exported, because C++ function 'HiddenNS::inHiddenNamespace' has hidden visibility}}{{3-10=}}
}

extension HiddenStruct {
  @cxx @implementation
  public func get() -> Int32 { value }
  // expected-warning@-1 {{instance method 'get()' is declared 'public' but will not be exported, because C++ function 'HiddenStruct::get' has hidden visibility}}{{3-10=}}
}

// Access inherited from the extension has no modifier to remove.
public extension HiddenRef {
  @cxx @implementation
  func describe() -> Int32 { value }
  // expected-warning@-1 {{instance method 'describe()' is declared 'public' but will not be exported, because C++ function 'HiddenRef::describe' has hidden visibility}}{{none}}
}
