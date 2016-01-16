// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -c -emit-module -emit-library -module-name superclass_properties -emit-module-path %t/superclass_properties.swiftmodule -Xfrontend -enable-resilience -D BEFORE %S/Inputs/superclass_properties.swift -o %t/libsuperclass_properties.dylib
// RUN: %target-build-swift %s -I %t -Xfrontend -lsuperclass_properties -L %t -Xfrontend -enable-resilience -o %t/main
// RUN: %t/main | FileCheck --check-prefix=BEFORE %s
// RUN: %target-build-swift -c -emit-module -emit-library -module-name superclass_properties -emit-module-path %t/superclass_properties.swiftmodule -Xfrontend -enable-resilience -D AFTER %S/Inputs/superclass_properties.swift -o %t/libsuperclass_properties.dylib
// RUN: %t/main | FileCheck --check-prefix=AFTER %s

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
