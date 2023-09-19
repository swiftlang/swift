// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-emit-ir -I %t/Inputs -enable-experimental-cxx-interop %t/test.swift -Xcc -fignore-exceptions | %FileCheck %t/test.swift


//--- Inputs/module.modulemap
module namespaces {
  header "test.h"
  requires cplusplus
}
//--- Inputs/test.h
namespace Parent {
inline namespace InlineChild {

void functionInInlineChild();

template<class CharT>
struct TemplateInInlineChild {
};

typedef TemplateInInlineChild<char> TypedefInInlineChild;

struct InInlineChild {
};

namespace NamespaceInInlineChild {

struct InNamespaceInInlineChild {
};

} // namespace NamespaceInInlineChild

inline namespace SecondInlineChild {

struct InSecondInlineChild {
};

} // namespace SecondInlineChild
} // namespace InlineChild
} // namespace Parent

//--- test.swift

import namespaces;

extension Parent.TypedefInInlineChild {
  var string: String {
    return ""
  }
}
// CHECK: define hidden swiftcc {{.*}} @"$sSo6ParentO11InlineChildO0034TemplateInInlineChildCChar_ckBDjAbV4testE6stringSSvg"()

extension Parent.InInlineChild {
  func doSomething() {
  }
}
// CHECK: define hidden swiftcc void @"$sSo6ParentO11InlineChildO02InbC0V4testE11doSomethingyyF"()

extension Parent.InSecondInlineChild {
  var x: Int {
    return 2
  }
}
// CHECK: define hidden swiftcc {{.*}} @"$sSo6ParentO11InlineChildO06SecondbC0O02IndbC0V4testE1xSivg"()

extension Parent.InlineChild.InSecondInlineChild {
  var y: Int {
    return 3
  }
}
// define hidden swiftcc {{.*}} @"$sSo6ParentO11InlineChildO06SecondbC0O02IndbC0V4testE1ySivg"()

// CHECK: define hidden swiftcc {{.*}} @"$s4test3useySSSo6ParentO11InlineChildO0034TemplateInInlineChildCChar_ckBDjAbVF"()
// CHECK: call swiftcc {{.*}} @"$sSo6ParentO11InlineChildO0034TemplateInInlineChildCChar_ckBDjAbV4testE6stringSSvg"
func use(_ x: Parent.TypedefInInlineChild) -> String {
  let s = x.string
  return s
}

// CHECK: define hidden swiftcc {{.*}} @"$s4test4use2ySSSo6ParentO11InlineChildO0034TemplateInInlineChildCChar_ckBDjAbVF"()
// CHECK: call swiftcc {{.*}} @"$sSo6ParentO11InlineChildO0034TemplateInInlineChildCChar_ckBDjAbV4testE6stringSSvg"
func use2(_ x: Parent.InlineChild.TypedefInInlineChild) -> String {
  let s = x.string
  return s
}

// define swiftcc void @"$s4testAAyySo6ParentO11InlineChildO02IncD0VF"()
// CHECK: alloca %TSo6ParentO11InlineChildO0034TemplateInInlineChildCChar_ckBDjAbV
// CHECK: call {{.*}} @{{_ZN6Parent11InlineChild21TemplateInInlineChildIcEC|"\?\?0\?\$TemplateInInlineChild@D@InlineChild@Parent@@QEAA@XZ"}}
// CHECK: call swiftcc void @"$sSo6ParentO11InlineChildO02InbC0V4testE11doSomethingyyF"(
// CHECK: call swiftcc {{.*}} @"$sSo6ParentO11InlineChildO06SecondbC0O02IndbC0V4testE1xSivg"(
// CHECK: call swiftcc {{.*}} @"$sSo6ParentO11InlineChildO06SecondbC0O02IndbC0V4testE1ySivg"(
// CHECK: call void @{{_ZN6Parent11InlineChild21functionInInlineChildEv|"\?functionInInlineChild@InlineChild@Parent@@YAXXZ"}}()
public func test(_ y: Parent.InlineChild.InInlineChild) {
  let s = Parent.TypedefInInlineChild()
  let s2 = use(s) + use2(s)
  y.doSomething()
  var i: Parent.InlineChild.SecondInlineChild.InSecondInlineChild?
  let i2 = i?.x
  let i3 = i?.y
  Parent.InlineChild.functionInInlineChild()
}

extension Parent.InlineChild {
  // CHECK: define hidden swiftcc void @"$sSo6ParentO11InlineChildO4testE011swiftFuncInB9NamespaceyyFZ"()
  static func swiftFuncInInlineNamespace() {
  }
}

// CHECK: define{{.*}} swiftcc void @"$s4test5test2yyF"()
// CHECK: call swiftcc void @"$sSo6ParentO11InlineChildO4testE011swiftFuncInB9NamespaceyyFZ"()
public func test2() {
  Parent.InlineChild.swiftFuncInInlineNamespace()
}

// CHECK: define hidden swiftcc void @"$sSo6ParentO11InlineChildO011NamespaceInbC0O0edebC0V4testE15doSomethingElseyyF"()
extension Parent.NamespaceInInlineChild.InNamespaceInInlineChild {
  func doSomethingElse() {}
}

// CHECK: define{{.*}} swiftcc void @"$s4test5test3yySo6ParentO11InlineChildO011NamespaceIndE0O0gfgdE0VF"()
// CHECK: call swiftcc void @"$sSo6ParentO11InlineChildO011NamespaceInbC0O0edebC0V4testE15doSomethingElseyyF"()
public func test3(_ x: Parent.InlineChild.NamespaceInInlineChild.InNamespaceInInlineChild) {
  x.doSomethingElse()
}
