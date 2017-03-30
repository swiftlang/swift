// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks %s 2>&1 | %FileCheck %s

// RUN: echo '#import <PrivatelyReadwrite/Private.h>' > %t.h
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -import-objc-header %t.h %s 2>&1 | %FileCheck %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-pch -F %S/Inputs/frameworks -o %t.pch %t.h
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -import-objc-header %t.pch %s 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

import PrivatelyReadwrite

// In the original bug, it made a difference whether the type was instantiated;
// it resulted in members being imported in a different order.
func testWithInitializer() {
  let obj = PropertiesInit()

  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'
}

func testWithoutInitializer(obj: PropertiesNoInit) {
  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'
}

func testCategoryWithInitializer() {
  let obj = PropertiesInitCategory()

  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'
}

func testCategoryWithoutInitializer(obj: PropertiesNoInitCategory) {
  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'
}
