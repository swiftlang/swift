// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after
// RUN: %target-build-swift -emit-module -emit-library -module-name superclass_methods -emit-module-path %t/superclass_methods.swiftmodule -Xfrontend -enable-resilience -D BEFORE %S/Inputs/superclass_methods.swift -o %t/before/superclass_methods.o
// RUN: %target-build-swift -c %s -I %t -Xfrontend -enable-resilience -o %t/main.o
// RUN: %target-build-swift %t/before/superclass_methods.o %t/main.o -o %t/before/main
// RUN: %t/before/main | FileCheck --check-prefix=BEFORE %s
// RUN: %target-build-swift -emit-module -emit-library -module-name superclass_methods -emit-module-path %t/superclass_methods.swiftmodule -Xfrontend -enable-resilience -D AFTER %S/Inputs/superclass_methods.swift -o %t/after/superclass_methods.o
// RUN: %target-build-swift %t/after/superclass_methods.o %t/main.o -o %t/after/main -v
// RUN: %t/after/main | FileCheck --check-prefix=AFTER %s

import superclass_methods

do {
  class Leaf : AddInterposingMethod {
    override func method() -> String {
      return super.method()
    }
    override class func classMethod() -> String {
      return super.classMethod()
    }
  }
  let leaf = Leaf()
  print(leaf.method())
  print(Leaf.classMethod())
  // BEFORE: Base.method()
  // BEFORE: Base.classMethod()
  // AFTER: AddInterposingMethod.method()
  // AFTER: AddInterposingMethod.classMethod()
}

do {
  class Leaf : RemoveInterposingMethod {
    override func method() -> String {
      return super.method()
    }
    override class func classMethod() -> String {
      return super.classMethod()
    }
  }
  print(Leaf().method())
  print(Leaf.classMethod())
  // BEFORE: RemoveInterposingMethod.method()
  // BEFORE: RemoveInterposingMethod.classMethod()
  // AFTER: Base.method()
  // AFTER: Base.classMethod()
}

do {
  class Leaf : InsertSuperclass {
    override func method() -> String {
      return super.method()
    }
    override class func classMethod() -> String {
      return super.classMethod()
    }
  }
  print(Leaf().method())
  print(Leaf.classMethod())
  // BEFORE: Base.method()
  // BEFORE: Base.classMethod()
  // AFTER: InBetween.method()
  // AFTER: InBetween.classMethod()
}

do {
  class Leaf : ChangeRoot {
    override func method() -> String {
      return super.method()
    }
    override class func classMethod() -> String {
      return super.classMethod()
    }
  }
  print(Leaf().method())
  print(Leaf.classMethod())
  // BEFORE: Base.method()
  // BEFORE: Base.classMethod()
  // AFTER: OtherBase.method()
  // AFTER: OtherBase.classMethod()
}
