// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/frameworks %s 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PUBLIC %s

// RUN: echo '#import <CategoryOverrides/Private.h>' > %t.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.h %s 2>&1 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PRIVATE %s --allow-empty

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-pch -F %S/Inputs/frameworks -o %t.pch %t.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.pch %s 2>&1 | %FileCheck --allow-empty -check-prefix=CHECK -check-prefix=CHECK-PRIVATE %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks -enable-objc-interop -import-objc-header %t.h -pch-output-dir %t/pch %s 2>&1 | %FileCheck --allow-empty -check-prefix=CHECK -check-prefix=CHECK-PRIVATE %s

import CategoryOverrides

// Nail down some emergent behaviors of the Clang Importer's override checking:


// A category declared in a (private) header can happen to double-import a property
// and a function with the same name - both before and after omit-needless-words -
// as long as they have different contextual types.
//
// This configuration appears as an undiagnosed redeclaration of a property and
// function, which is illegal.
func colors() {
  // CHECK-PUBLIC: cannot call value of non-function type 'MyColor?'
  // CHECK-PRIVATE-NOT: cannot call value of non-function type 'MyColor?'
  let _ : MyColor = MyColor.systemRed
  let _ : MyColor = MyColor.systemRed()!
}

// Another manifestation of the above for an instance property this time.
func structs(_ base: MyBaseClass, _ derived: MyDerivedClass) {
  // CHECK-PUBLIC: cannot call value of non-function type 'SomeStruct'
  // CHECK-PRIVATE-NOT: cannot call value of non-function type 'SomeStruct'
  let _ : SomeStruct = base.myStructure
  let _ : SomeStruct = base.myStructure()

  // CHECK-PUBLIC: cannot call value of non-function type 'SomeStruct'
  // CHECK-PRIVATE-NOT: cannot call value of non-function type 'SomeStruct'
  let _ : SomeStruct = derived.myStructure
  let _ : SomeStruct = derived.myStructure()
}

// A category declared in a (private) header can introduce overrides of a property
// that is otherwise not declared in a base class.
//
// This configuration appears as an undiagnosed override in a Swift extension,
// which is illegal.
func takesADerivedClass(_ x: MyDerivedClass) {
  // CHECK-PUBLIC-NOT: has no member 'derivedMember'
  // CHECK-PRIVATE-NOT: has no member 'derivedMember'
  x.derivedMember = Base()
}

func takesABaseClass(_ x: MyBaseClass) {
  // CHECK-PUBLIC: has no member 'derivedMember'
  // CHECK-PRIVATE-NOT: has no member 'derivedMember'
  x.derivedMember = Base()
}
