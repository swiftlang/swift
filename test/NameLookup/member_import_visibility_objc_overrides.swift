// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck \
// RUN:   -primary-file %t/file1.swift \
// RUN:   -primary-file %t/file2.swift \
// RUN:   -primary-file %t/file3.swift \
// RUN:   -primary-file %t/file4.swift \
// RUN:   -primary-file %t/file5.swift \
// RUN:   -I %S/Inputs/MemberImportVisibility/ObjCOverloads \
// RUN:   -verify -verify-additional-prefix no-member-visibility-

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck \
// RUN:   -primary-file %t/file1.swift \
// RUN:   -primary-file %t/file2.swift \
// RUN:   -primary-file %t/file3.swift \
// RUN:   -primary-file %t/file4.swift \
// RUN:   -primary-file %t/file5.swift \
// RUN:   -I %S/Inputs/MemberImportVisibility/ObjCOverloads \
// RUN:   -enable-upcoming-feature MemberImportVisibility \
// RUN:   -verify -verify-additional-prefix member-visibility-

// REQUIRES: objc_interop
// REQUIRES: swift_feature_MemberImportVisibility

//--- file1.swift

import Root
// expected-member-visibility-note 3 {{add import of module 'Branch'}}
// expected-member-visibility-note@-1 2 {{add import of module 'Leaf'}}
// expected-member-visibility-note@-2 2 {{add import of module 'Fruit'}}

func testImportRoot_overridden1() {
  makeRootObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Root.h}}

  makeBranchObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Branch.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden1()' is not available due to missing import of defining module 'Branch'}}

  makeLeafObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Leaf.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden1()' is not available due to missing import of defining module 'Leaf'}}

  makeFruitObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Fruit.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden1()' is not available due to missing import of defining module 'Fruit'}}
}

func testImportRoot_overridden2() {
  makeRootObject().overridden2()
  // expected-warning@-1 {{'overridden2()' is deprecated: Root.h}}

  makeBranchObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}

  makeLeafObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}

  makeFruitObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}
}

func testImportRoot_overridden3() {
  makeRootObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Root.h}}

  makeBranchObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Branch.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden3()' is not available due to missing import of defining module 'Branch'}}

  makeLeafObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Leaf.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden3()' is not available due to missing import of defining module 'Leaf'}}

  makeFruitObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Branch.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden3()' is not available due to missing import of defining module 'Branch'}}
}

func testImportRoot_overridden4() {
  makeRootObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Root.h}}

  makeBranchObject().overridden4()
  // expected-no-member-visibility-warning@-1 {{'overridden4()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden4()' is deprecated: Root.h}}

  makeLeafObject().overridden4()
  // expected-no-member-visibility-warning@-1 {{'overridden4()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden4()' is deprecated: Root.h}}

  makeFruitObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Fruit.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden4()' is not available due to missing import of defining module 'Fruit'}}
}


//--- file2.swift

import Branch
// expected-member-visibility-note 2 {{add import of module 'Leaf'}}
// expected-member-visibility-note@-1 2 {{add import of module 'Fruit'}}

func testImportBranch_overridden1() {
  makeRootObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Root.h}}

  makeBranchObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Branch.h}}

  makeLeafObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Leaf.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden1()' is not available due to missing import of defining module 'Leaf'}}

  makeFruitObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Fruit.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden1()' is not available due to missing import of defining module 'Fruit'}}
}

func testImportBranch_overridden2() {
  makeRootObject().overridden2()
  // expected-warning@-1 {{'overridden2()' is deprecated: Root.h}}

  makeBranchObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}

  makeLeafObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}

  makeFruitObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}
}

func testImportBranch_overridden3() {
  makeRootObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Root.h}}

  makeBranchObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Branch.h}}

  makeLeafObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Leaf.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden3()' is not available due to missing import of defining module 'Leaf'}}

  makeFruitObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Branch.h}}
}

func testImportBranch_overridden4() {
  makeRootObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Root.h}}

  makeBranchObject().overridden4()
  // expected-no-member-visibility-warning@-1 {{'overridden4()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden4()' is deprecated: Root.h}}

  makeLeafObject().overridden4()
  // expected-no-member-visibility-warning@-1 {{'overridden4()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden4()' is deprecated: Root.h}}

  makeFruitObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Fruit.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden4()' is not available due to missing import of defining module 'Fruit'}}
}


//--- file3.swift

import Leaf
// expected-member-visibility-note 2 {{add import of module 'Fruit'}}

func testImportLeaf_overridden1() {
  makeRootObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Root.h}}

  makeBranchObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Branch.h}}

  makeLeafObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Leaf.h}}

  makeFruitObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Fruit.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden1()' is not available due to missing import of defining module 'Fruit'}}
}

func testImportLeaf_overridden2() {
  makeRootObject().overridden2()
  // expected-warning@-1 {{'overridden2()' is deprecated: Root.h}}

  makeBranchObject().overridden2()
  // expected-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}

  makeLeafObject().overridden2()
  // expected-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}

  makeFruitObject().overridden2()
  // expected-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
}

func testImportLeaf_overridden3() {
  makeRootObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Root.h}}

  makeBranchObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Branch.h}}

  makeLeafObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Leaf.h}}

  makeFruitObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Branch.h}}
}

func testImportLeaf_overridden4() {
  makeRootObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Root.h}}

  makeBranchObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Leaf.h}}

  makeLeafObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Leaf.h}}

  makeFruitObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Fruit.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden4()' is not available due to missing import of defining module 'Fruit'}}
}


//--- file4.swift

import Fruit
// expected-member-visibility-note 2 {{add import of module 'Leaf'}}

func testImportFruit_overridden1() {
  makeRootObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Root.h}}

  makeBranchObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Branch.h}}

  makeLeafObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Leaf.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden1()' is not available due to missing import of defining module 'Leaf'}}

  makeFruitObject().overridden1()
  // expected-warning@-1 {{'overridden1()' is deprecated: Fruit.h}}
}

func testImportFruit_overridden2() {
  makeRootObject().overridden2()
  // expected-warning@-1 {{'overridden2()' is deprecated: Root.h}}

  makeBranchObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}

  makeLeafObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}

  makeFruitObject().overridden2()
  // expected-no-member-visibility-warning@-1 {{'overridden2()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden2()' is deprecated: Root.h}}
}

func testImportFruit_overridden3() {
  makeRootObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Root.h}}

  makeBranchObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Branch.h}}

  makeLeafObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Leaf.h}}
  // expected-member-visibility-error@-2 {{instance method 'overridden3()' is not available due to missing import of defining module 'Leaf'}}

  makeFruitObject().overridden3()
  // expected-warning@-1 {{'overridden3()' is deprecated: Branch.h}}
}

func testImportFruit_overridden4() {
  makeRootObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Root.h}}

  makeBranchObject().overridden4()
  // expected-no-member-visibility-warning@-1 {{'overridden4()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden4()' is deprecated: Root.h}}

  makeLeafObject().overridden4()
  // expected-no-member-visibility-warning@-1 {{'overridden4()' is deprecated: Leaf.h}}
  // expected-member-visibility-warning@-2 {{'overridden4()' is deprecated: Root.h}}

  makeFruitObject().overridden4()
  // expected-warning@-1 {{'overridden4()' is deprecated: Fruit.h}}
}


//--- file5.swift

import Root
import Branch
import Leaf
import Fruit

func makeRootObject() -> RootObject { RootObject() }
func makeBranchObject() -> BranchObject { BranchObject() }
func makeLeafObject() -> LeafObject { LeafObject() }
func makeFruitObject() -> FruitObject { FruitObject() }
