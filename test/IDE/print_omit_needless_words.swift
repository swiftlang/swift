// RUN: rm -rf %t
// RUN: mkdir -p %t

// REQUIRES: objc_interop

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -enable-omit-needless-words %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -enable-omit-needless-words %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -enable-omit-needless-words %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -enable-omit-needless-words %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=ObjectiveC -function-definitions=false -prefer-type-repr=true -enable-omit-needless-words > %t.ObjectiveC.txt
// RUN: FileCheck %s -check-prefix=CHECK-OBJECTIVEC -strict-whitespace < %t.ObjectiveC.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=Foundation -function-definitions=false -prefer-type-repr=true -enable-omit-needless-words > %t.Foundation.txt
// RUN: FileCheck %s -check-prefix=CHECK-FOUNDATION -strict-whitespace < %t.Foundation.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=AppKit -function-definitions=false -prefer-type-repr=true -enable-omit-needless-words > %t.AppKit.txt
// RUN: FileCheck %s -check-prefix=CHECK-APPKIT -strict-whitespace < %t.AppKit.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t -I %S/../ClangModules/Inputs/custom-modules) -print-module -source-filename %s -module-to-print=CoreCooling -function-definitions=false -prefer-type-repr=true -enable-omit-needless-words > %t.CoreCooling.txt
// RUN: FileCheck %s -check-prefix=CHECK-CORECOOLING -strict-whitespace < %t.CoreCooling.txt

// Note: SEL -> "Selector"
// CHECK-FOUNDATION: func makeObjectsPerform(aSelector: Selector)

// Note: id -> "Object", leaving "with"
// CHECK-FOUNDATION: func makeObjectsPerform(aSelector: Selector, with anObject: AnyObject?)

// Note: Class -> "Class"
// CHECK-OBJECTIVEC: func isKindOf(aClass: AnyClass) -> Bool

// Note: Pointer-to-struct name matching; don't drop "With".
// CHECK-FOUNDATION: func copyWith(zone: NSZone) -> AnyObject!

// Note: Objective-C type parameter names.
// CHECK-FOUNDATION: func objectFor(aKey: NSCopying) -> AnyObject?
// CHECK-FOUNDATION: func removeObjectFor(aKey: NSCopying)

// Note: Allow argument labels that are keywords.
// CHECK-FOUNDATION: func setObject(anObject: AnyObject, `for` aKey: NSCopying)

// Note: Don't drop the name of the first parameter in an initializer entirely.
// CHECK-FOUNDATION: init(array: [AnyObject])

// Note: struct name matching; don't drop "With".
// CHECK-FOUNDATION: class func valueWith(range: NSRange) -> NSValue

// Note: built-in types.
// CHECK-FOUNDATION: func add(value: Double) -> NSNumber

// Note: multi-word enum name matching; don't drop "With".
// CHECK-FOUNDATION: func someMethodWith(options: NSDeprecatedOptions)

// Note: class name matching; don't drop "With".
// CHECK-FOUNDATION: class func requestWith(URLString: String!) -> Self!

// Note: property name stripping property type.
// CHECK-FOUNDATION: var uppercase: String

// Note: don't map base name down to a keyword.
// CHECK-FOUNDATION: func doSelector(selector: Selector)

// Note: Strip names preceded by a gerund.
// CHECK-FOUNDATION: func startSquashing(bee: Bee)
// CHECK-FOUNDATION: func startSoothing(bee: Bee)
// CHECK-FOUNDATION: func startShopping(bee: Bee)

// Note: Removing plural forms when working with collections
// CHECK-FOUNDATION: func add(objects: [AnyObject])

// Note: Int and Index match.
// CHECK-FOUNDATION: func sliceFrom(fromIndex: Int, to toIndex: Int) -> String

// Note: <result type>By<gerund> --> <gerund>.
// CHECK-FOUNDATION: func appending(string: String) -> String

// Note: <property type>By<gerund> --> <gerund>.
// CHECK-FOUNDATION: var deletingLastPathComponent: NSURL? { get }

// Note: class method name stripping result type.
// CHECK-APPKIT: class func red() -> NSColor

// Note: instance method name stripping result type.
// CHECK-APPKIT: func same() -> Self

// Note: Skipping over "3D"
// CHECK-APPKIT: func drawInAirAt(point: Point3D)

// Note: Don't strip names that aren't preceded by a verb or preposition.
// CHECK-APPKIT: func setTextColor(color: NSColor)

// Note: Skipping over "Ref"
// CHECK-CORECOOLING: func replace(powerSupply: CCPowerSupply!)
