// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after
// RUN: %target-build-swift -emit-module -emit-library -module-name superclass_properties -emit-module-path %t/superclass_properties.swiftmodule -Xfrontend -enable-resilience -D BEFORE %S/Inputs/superclass_properties.swift -o %t/before/superclass_properties.o
// RUN: %target-build-swift -c %s -I %t -Xfrontend -enable-resilience -o %t/main.o
// RUN: %target-build-swift %t/before/superclass_properties.o %t/main.o -o %t/before/main
// RUN: %t/before/main | FileCheck --check-prefix=BEFORE %s
// RUN: %target-build-swift -emit-module -emit-library -module-name superclass_properties -emit-module-path %t/superclass_properties.swiftmodule -Xfrontend -enable-resilience -D AFTER %S/Inputs/superclass_properties.swift -o %t/after/superclass_properties.o
// RUN: %target-build-swift %t/after/superclass_properties.o %t/main.o -o %t/after/main -v
// RUN: %t/after/main | FileCheck --check-prefix=AFTER %s

import superclass_properties

do {
  class Leaf : AddInterposingProperty {
    override var property: String {
      return super.property
    }
    override class var classProperty: String {
      return super.classProperty
    }
  }
  let leaf = Leaf()
  print(leaf.property)
  print(Leaf.classProperty)
  // BEFORE: Base.property
  // BEFORE: Base.classProperty
  // AFTER: AddInterposingProperty.property
  // AFTER: AddInterposingProperty.classProperty
}

do {
  class Leaf : RemoveInterposingProperty {
    override var property: String {
      return super.property
    }
    override class var classProperty: String {
      return super.classProperty
    }
  }
  print(Leaf().property)
  print(Leaf.classProperty)
  // BEFORE: RemoveInterposingProperty.property
  // BEFORE: RemoveInterposingProperty.classProperty
  // AFTER: Base.property
  // AFTER: Base.classProperty
}

do {
  class Leaf : InsertSuperclass {
    override var property: String {
      return super.property
    }
    override class var classProperty: String {
      return super.classProperty
    }
  }
  print(Leaf().property)
  print(Leaf.classProperty)
  // BEFORE: Base.property
  // BEFORE: Base.classProperty
  // AFTER: InBetween.property
  // AFTER: InBetween.classProperty
}

do {
  class Leaf : ChangeRoot {
    override var property: String {
      return super.property
    }
    override class var classProperty: String {
      return super.classProperty
    }
  }
  print(Leaf().property)
  print(Leaf.classProperty)
  // BEFORE: Base.property
  // BEFORE: Base.classProperty
  // AFTER: OtherBase.property
  // AFTER: OtherBase.classProperty
}
