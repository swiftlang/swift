// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop

//--- Inputs/module.modulemap
module namespaces {
  header "test.h"
  requires cplusplus
}
//--- Inputs/test.h
inline namespace TopLevelInline {

struct InTopLevelInline {
};

} // namespace TopLevelInline

//--- test.swift

import namespaces;

extension InTopLevelInline { // expected-error {{cannot find type 'InTopLevelInline' in scope}}
}

extension TopLevelInline.InTopLevelInline { // ok
  var string: String {
    return ""
  }
}
