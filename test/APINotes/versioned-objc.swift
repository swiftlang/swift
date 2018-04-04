// RUN: %empty-directory(%t)

// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 4 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-4 %s
// RUN: not %target-swift-frontend -typecheck -F %S/Inputs/custom-frameworks -swift-version 3 %s 2>&1 | %FileCheck -check-prefix=CHECK-DIAGS -check-prefix=CHECK-DIAGS-3 %s

// REQUIRES: objc_interop

import APINotesFrameworkTest

// CHECK-DIAGS-4-NOT: versioned-objc.swift:[[@LINE-1]]:
class ProtoWithVersionedUnavailableMemberImpl: ProtoWithVersionedUnavailableMember {
  // CHECK-DIAGS-3: versioned-objc.swift:[[@LINE-1]]:7: error: type 'ProtoWithVersionedUnavailableMemberImpl' cannot conform to protocol 'ProtoWithVersionedUnavailableMember' because it has requirements that cannot be satisfied
  func requirement() -> Any? { return nil }
}

func testNonGeneric() {
  // CHECK-DIAGS-3:[[@LINE+1]]:{{[0-9]+}}: error: cannot convert value of type 'Any' to specified type 'Int'
  let _: Int = NewlyGenericSub.defaultElement()
  // CHECK-DIAGS-4:[[@LINE-1]]:{{[0-9]+}}: error: generic parameter 'Element' could not be inferred

  // CHECK-DIAGS-3:[[@LINE+1]]:{{[0-9]+}}: error: cannot specialize non-generic type 'NewlyGenericSub'
  let _: Int = NewlyGenericSub<Base>.defaultElement()
  // CHECK-DIAGS-4:[[@LINE-1]]:{{[0-9]+}}: error: cannot convert value of type 'Base' to specified type 'Int'
}

func testRenamedGeneric() {
  // CHECK-DIAGS-3-NOT: 'RenamedGeneric' has been renamed to 'OldRenamedGeneric'
  let _: OldRenamedGeneric<Base> = RenamedGeneric<Base>()
  // CHECK-DIAGS-4:[[@LINE-1]]:{{[0-9]+}}: error: 'OldRenamedGeneric' has been renamed to 'RenamedGeneric'

  // CHECK-DIAGS-3-NOT: 'RenamedGeneric' has been renamed to 'OldRenamedGeneric'
  let _: RenamedGeneric<Base> = OldRenamedGeneric<Base>()
  // CHECK-DIAGS-4:[[@LINE-1]]:{{[0-9]+}}: error: 'OldRenamedGeneric' has been renamed to 'RenamedGeneric'

  class SwiftClass {}

  // CHECK-DIAGS-3:[[@LINE+1]]:{{[0-9]+}}: error: 'RenamedGeneric' requires that 'SwiftClass' inherit from 'Base'
  let _: OldRenamedGeneric<SwiftClass> = RenamedGeneric<SwiftClass>()
  // CHECK-DIAGS-4:[[@LINE-1]]:{{[0-9]+}}: error: 'RenamedGeneric' requires that 'SwiftClass' inherit from 'Base'

  // CHECK-DIAGS-3:[[@LINE+1]]:{{[0-9]+}}: error: 'RenamedGeneric' requires that 'SwiftClass' inherit from 'Base'
  let _: RenamedGeneric<SwiftClass> = OldRenamedGeneric<SwiftClass>()
  // CHECK-DIAGS-4:[[@LINE-1]]:{{[0-9]+}}: error: 'RenamedGeneric' requires that 'SwiftClass' inherit from 'Base'
}

func testRenamedClassMembers(obj: ClassWithManyRenames) {
  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'classWithManyRenamesForInt' has been replaced by 'init(swift3Factory:)'
  _ = ClassWithManyRenames.classWithManyRenamesForInt(0)
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'classWithManyRenamesForInt' has been replaced by 'init(for:)'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'init(forInt:)' has been renamed to 'init(swift3Factory:)'
  _ = ClassWithManyRenames(forInt: 0)
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'init(forInt:)' has been renamed to 'init(for:)'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  _ = ClassWithManyRenames(swift3Factory: 0)
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'init(swift3Factory:)' has been renamed to 'init(for:)'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'init(for:)' has been renamed to 'init(swift3Factory:)'
  _ = ClassWithManyRenames(for: 0)
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:


  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'init(boolean:)' has been renamed to 'init(swift3Boolean:)'
  _ = ClassWithManyRenames(boolean: false)
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'init(boolean:)' has been renamed to 'init(finalBoolean:)'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  _ = ClassWithManyRenames(swift3Boolean: false)
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'init(swift3Boolean:)' has been renamed to 'init(finalBoolean:)'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'init(finalBoolean:)' has been renamed to 'init(swift3Boolean:)'
  _ = ClassWithManyRenames(finalBoolean: false)
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:


  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'doImportantThings()' has been renamed to 'swift3DoImportantThings()'
  obj.doImportantThings()
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'doImportantThings()' has been renamed to 'finalDoImportantThings()'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  obj.swift3DoImportantThings()
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'swift3DoImportantThings()' has been renamed to 'finalDoImportantThings()'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'finalDoImportantThings()' has been renamed to 'swift3DoImportantThings()'
  obj.finalDoImportantThings()
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:


  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'importantClassProperty' has been renamed to 'swift3ClassProperty'
  _ = ClassWithManyRenames.importantClassProperty
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'importantClassProperty' has been renamed to 'finalClassProperty'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  _ = ClassWithManyRenames.swift3ClassProperty
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'swift3ClassProperty' has been renamed to 'finalClassProperty'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'finalClassProperty' has been renamed to 'swift3ClassProperty'
  _ = ClassWithManyRenames.finalClassProperty
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:
}

func testRenamedProtocolMembers(obj: ProtoWithManyRenames) {
  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'init(boolean:)' has been renamed to 'init(swift3Boolean:)'
  _ = type(of: obj).init(boolean: false)
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'init(boolean:)' has been renamed to 'init(finalBoolean:)'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  _ = type(of: obj).init(swift3Boolean: false)
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'init(swift3Boolean:)' has been renamed to 'init(finalBoolean:)'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'init(finalBoolean:)' has been renamed to 'init(swift3Boolean:)'
  _ = type(of: obj).init(finalBoolean: false)
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:


  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'doImportantThings()' has been renamed to 'swift3DoImportantThings()'
  obj.doImportantThings()
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'doImportantThings()' has been renamed to 'finalDoImportantThings()'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  obj.swift3DoImportantThings()
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'swift3DoImportantThings()' has been renamed to 'finalDoImportantThings()'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'finalDoImportantThings()' has been renamed to 'swift3DoImportantThings()'
  obj.finalDoImportantThings()
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:


  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'importantClassProperty' has been renamed to 'swift3ClassProperty'
  _ = type(of: obj).importantClassProperty
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'importantClassProperty' has been renamed to 'finalClassProperty'

  // CHECK-DIAGS-3-NOT: :[[@LINE+1]]:{{[0-9]+}}:
  _ = type(of: obj).swift3ClassProperty
  // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: 'swift3ClassProperty' has been renamed to 'finalClassProperty'

  // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: error: 'finalClassProperty' has been renamed to 'swift3ClassProperty'
  _ = type(of: obj).finalClassProperty
  // CHECK-DIAGS-4-NOT: :[[@LINE-1]]:{{[0-9]+}}:
}

extension PrintingRenamed {
  func testDroppingRenamedPrints() {
    // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: warning: use of 'print' treated as a reference to instance method
    print()
    // CHECK-DIAGS-4-NOT: [[@LINE-1]]:{{[0-9]+}}:

    // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: warning: use of 'print' treated as a reference to instance method
    print(self)
    // CHECK-DIAGS-4-NOT: [[@LINE-1]]:{{[0-9]+}}:
  }

  static func testDroppingRenamedPrints() {
    // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: warning: use of 'print' treated as a reference to class method
    print()
    // CHECK-DIAGS-4-NOT: [[@LINE-1]]:{{[0-9]+}}:

    // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: warning: use of 'print' treated as a reference to class method
    print(self)
    // CHECK-DIAGS-4-NOT: [[@LINE-1]]:{{[0-9]+}}:
  }
}

extension PrintingInterference {
  func testDroppingRenamedPrints() {
    // CHECK-DIAGS-3: [[@LINE+1]]:{{[0-9]+}}: warning: use of 'print' treated as a reference to instance method
    print(self)
    // CHECK-DIAGS-4: [[@LINE-1]]:{{[0-9]+}}: error: use of 'print' nearly matches global function 'print(_:separator:terminator:)' in module 'Swift' rather than instance method 'print(_:extra:)'

    // CHECK-DIAGS-3-NOT: [[@LINE+1]]:{{[0-9]+}}:
    print(self, extra: self)
    // CHECK-DIAGS-4-NOT: [[@LINE-1]]:{{[0-9]+}}:
  }
}

let unrelatedDiagnostic: Int = nil
