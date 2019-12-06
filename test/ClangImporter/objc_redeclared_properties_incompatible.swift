// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/frameworks %s 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PUBLIC %s

// RUN: echo '#import <PrivatelyReadwrite/Private.h>' > %t.h
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.h %s 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PRIVATE %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-pch -F %S/Inputs/frameworks -o %t.pch %t.h
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.pch %s 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PRIVATE %s
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.h -pch-output-dir %t/pch %s 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PRIVATE %s

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

  obj.readwriteChange = Base() // CHECK-PRIVATE-NOT: [[@LINE]]:{{.+}}: error
  // CHECK-PUBLIC: [[@LINE-1]]:7: error: cannot assign to property: 'readwriteChange' is a get-only property
}

func testWithoutInitializer(obj: PropertiesNoInit) {
  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  obj.readwriteChange = Base() // CHECK-PRIVATE-NOT: [[@LINE]]:{{.+}}: error
  // CHECK-PUBLIC: [[@LINE-1]]:7: error: cannot assign to property: 'readwriteChange' is a get-only property
}

func testGenericWithInitializer() {
  let obj = PropertiesInitGeneric<Base>()

  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  obj.readwriteChange = Base() // CHECK-PRIVATE-NOT: [[@LINE]]:{{.+}}: error
  // CHECK-PUBLIC: [[@LINE-1]]:7: error: cannot assign to property: 'readwriteChange' is a get-only property
}

func testGenericWithoutInitializer(obj: PropertiesNoInitGeneric<Base>) {
  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  obj.readwriteChange = Base() // CHECK-PRIVATE-NOT: [[@LINE]]:{{.+}}: error
  // CHECK-PUBLIC: [[@LINE-1]]:7: error: cannot assign to property: 'readwriteChange' is a get-only property
}

func testCategoryWithInitializer() {
  let obj = PropertiesInitCategory()

  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  obj.readwriteChange = Base() // CHECK-PRIVATE-NOT: [[@LINE]]:{{.+}}: error
  // CHECK-PUBLIC: [[@LINE-1]]:7: error: cannot assign to property: 'readwriteChange' is a get-only property
}

func testCategoryWithoutInitializer(obj: PropertiesNoInitCategory) {
  let _: Int = obj.nullabilityChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  let _: Int = obj.missingGenerics
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'GenericClass<Base>' to specified type 'Int'

  let _: Int = obj.typeChange
  // CHECK: [[@LINE-1]]:20: error: cannot convert value of type 'Base' to specified type 'Int'

  obj.readwriteChange = Base() // CHECK-PRIVATE-NOT: [[@LINE]]:{{.+}}: error
  // CHECK-PUBLIC: [[@LINE-1]]:7: error: cannot assign to property: 'readwriteChange' is a get-only property
}
