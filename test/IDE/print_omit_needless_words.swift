// RUN: rm -rf %t
// RUN: mkdir -p %t

// REQUIRES: objc_interop

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -enable-omit-needless-words %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -enable-omit-needless-words %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -enable-omit-needless-words %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -enable-omit-needless-words %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=ObjectiveC -function-definitions=false -prefer-type-repr=true -enable-omit-needless-words -enable-infer-default-arguments > %t.ObjectiveC.txt
// RUN: FileCheck %s -check-prefix=CHECK-OBJECTIVEC -strict-whitespace < %t.ObjectiveC.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=Foundation -function-definitions=false -prefer-type-repr=true -enable-omit-needless-words -skip-parameter-names -enable-infer-default-arguments > %t.Foundation.txt
// RUN: FileCheck %s -check-prefix=CHECK-FOUNDATION -strict-whitespace < %t.Foundation.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=AppKit -function-definitions=false -prefer-type-repr=true -enable-omit-needless-words -skip-parameter-names -enable-infer-default-arguments > %t.AppKit.txt
// RUN: FileCheck %s -check-prefix=CHECK-APPKIT -strict-whitespace < %t.AppKit.txt

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t -I %S/../ClangModules/Inputs/custom-modules) -print-module -source-filename %s -module-to-print=CoreCooling -function-definitions=false -prefer-type-repr=true -enable-omit-needless-words -skip-parameter-names -enable-infer-default-arguments > %t.CoreCooling.txt
// RUN: FileCheck %s -check-prefix=CHECK-CORECOOLING -strict-whitespace < %t.CoreCooling.txt

// Note: SEL -> "Selector"
// CHECK-FOUNDATION: func makeObjectsPerform(_: Selector)

// Note: "with" parameters.
// CHECK-FOUNDATION: func makeObjectsPerform(_: Selector, withObject: AnyObject?)
// CHECK-FOUNDATION: func makeObjectsPerform(_: Selector, withObject: AnyObject?, withObject: AnyObject?)

// Note: id -> "Object".
// CHECK-FOUNDATION: func indexOf(_: AnyObject) -> Int

// Note: Class -> "Class"
// CHECK-OBJECTIVEC: func isKindOf(aClass: AnyClass) -> Bool

// Note: Pointer-to-struct name matching; "with" splits the first
// piece, then the "with" is dropped.
//
// CHECK-FOUNDATION: func copy(zone _: NSZone = nil) -> AnyObject!

// Note: Objective-C type parameter names.
// CHECK-FOUNDATION: func objectFor(_: NSCopying) -> AnyObject?
// CHECK-FOUNDATION: func removeObjectFor(_: NSCopying)

// Note: Don't drop the name of the first parameter in an initializer entirely.
// CHECK-FOUNDATION: init(array: [AnyObject])

// Note: struct name matching; don't drop "With".
// CHECK-FOUNDATION: class func withRange(_: NSRange) -> NSValue

// Note: built-in types.
// CHECK-FOUNDATION: func add(_: Double) -> NSNumber

// Note: built-in types.
// CHECK-FOUNDATION: func add(_: Bool) -> NSNumber

// Note: builtin-types.
// CHECK-FOUNDATION: func add(_: UInt16) -> NSNumber

// Note: builtin-types.
// CHECK-FOUNDATION: func add(_: Int32) -> NSNumber

// Note: Typedefs with a "_t" suffix".
// CHECK-FOUNDATION: func subtract(_: Int32) -> NSNumber

// Note: Respect the getter name for BOOL properties.
// CHECK-FOUNDATION: var isMakingHoney: Bool

// Note: multi-word enum name matching; "with" splits the first piece.
// CHECK-FOUNDATION: func someMethod(deprecatedOptions _: NSDeprecatedOptions = [])

// Note: class name matching; don't drop "With".
// CHECK-FOUNDATION: class func withString(_: String!) -> Self!

// Note: Make sure NSURL works in various places
// CHECK-FOUNDATION: open(_: NSURL!, completionHandler: ((Bool) -> Void)!)

// Note: property name stripping property type.
// CHECK-FOUNDATION: var uppercase: String

// Note: don't map base name down to a keyword.
// CHECK-FOUNDATION: func doSelector(_: Selector)

// Note: Strip names preceded by a gerund.
// CHECK-FOUNDATION: func startSquashing(_: Bee)
// CHECK-FOUNDATION: func startSoothing(_: Bee)
// CHECK-FOUNDATION: func startShopping(_: Bee)

// Note: Removing plural forms when working with collections
// CHECK-FOUNDATION: func add(_: [AnyObject])

// Note: Int and Index match.
// CHECK-FOUNDATION: func sliceFrom(_: Int, to: Int) -> String

// Note: <context type>By<gerund> --> <gerund>.
// CHECK-FOUNDATION: func appending(_: String) -> String

// Note: <context type>By<gerund> --> <gerund>.
// CHECK-FOUNDATION: func withString(_: String) -> String

// Note: Not splitting on "With".
// CHECK-FOUNDATION: func urlWithAddedString(_: String) -> NSURL?

// Note: CalendarUnits is not a set of "Options".
// CHECK-FOUNDATION: class func forCalendarUnits(_: NSCalendarUnit) -> String!

// Note: <property type>By<gerund> --> <gerund>.
// CHECK-FOUNDATION: var deletingLastPathComponent: NSURL? { get }

// Note: <property type><preposition> --> <preposition>.
// CHECK-FOUNDATION: var withHTTPS: NSURL { get }

// Note: usingBlock -> body
// CHECK-FOUNDATION: func enumerateObjectsUsing(_: ((AnyObject!, Int, UnsafeMutablePointer<ObjCBool>) -> Void)!)
// CHECK-FOUNDATION: func enumerateObjects(options _: NSEnumerationOptions = [], usingBlock: ((AnyObject!, Int, UnsafeMutablePointer<ObjCBool>) -> Void)!)

// Note: WithBlock -> body, nullable closures default to nil.
// CHECK-FOUNDATION: func enumerateObjectsRandomly(block _: ((AnyObject!, Int, UnsafeMutablePointer<ObjCBool>) -> Void)? = nil)

// Note: id<Proto> treated as "Proto".
// CHECK-FOUNDATION: func doSomethingWith(_: NSCopying)

// Note: NSObject<Proto> treated as "Proto".
// CHECK-FOUNDATION: func doSomethingElseWith(_: protocol<NSCopying, NSObjectProtocol>)

// Note: Function type -> "Function".
// CHECK-FOUNDATION: func sortUsing(_: @convention(c) (AnyObject, AnyObject) -> Int)

// Note: Plural: NSArray without type arguments -> "Objects".
// CHECK-FOUNDATION: func remove(_: [AnyObject])

// Note: Skipping "Type" suffix.
// CHECK-FOUNDATION: func doSomethingWith(_: NSUnderlyingType)

// Don't introduce default arguments for lone parameters to setters.
// CHECK-FOUNDATION: func setDefaultEnumerationOptions(_: NSEnumerationOptions)

// CHECK-FOUNDATION: func normalizingXMLPreservingComments(_: Bool)

// Collection element types.
// CHECK-FOUNDATION: func adding(_: AnyObject) -> Set<NSObject>

// Boolean properties follow the getter.
// CHECK-FOUNDATION: var empty: Bool { get }
// CHECK-FOUNDATION: func nonEmpty() -> Bool
// CHECK-FOUNDATION: var isStringSet: Bool { get }
// CHECK-FOUNDATION: var wantsAUnion: Bool { get }
// CHECK-FOUNDATION: var watchesItsLanguage: Bool { get }
// CHECK-FOUNDATION: var appliesForAJob: Bool { get }
// CHECK-FOUNDATION: var setShouldBeInfinite: Bool { get }

// "UTF8" initialisms.
// CHECK-FOUNDATION: init?(utf8String: UnsafePointer<Int8>)

// Note: class method name stripping context type.
// CHECK-APPKIT: class func red() -> NSColor

// Note: instance method name stripping context type.
// CHECK-APPKIT: func same() -> Self

// Note: Unsafe(Mutable)Pointers don't get defaulted to 'nil'
// CHECK-APPKIT: func getRGBAComponents(_: UnsafeMutablePointer<Int8>)

// Note: Skipping over "3D"
// CHECK-APPKIT: func drawInAirAt(_: Point3D)

// Note: with<something> -> <something>
// CHECK-APPKIT: func drawAt(_: Point3D, withAttributes: [String : AnyObject]? = [:])

// Note: Don't strip names that aren't preceded by a verb or preposition.
// CHECK-APPKIT: func setTextColor(_: NSColor?)

// Note: Splitting with default arguments.
// CHECK-APPKIT: func drawIn(_: NSView?)

// Note: NSDictionary default arguments for "options"
// CHECK-APPKIT: func drawAnywhereIn(_: NSView?, options: [NSObject : AnyObject] = [:])
// CHECK-APPKIT: func drawAnywhere(options _: [NSObject : AnyObject] = [:])

// Note: Skipping over "Ref"
// CHECK-CORECOOLING: func replace(_: CCPowerSupply!)

// Make sure we're removing redundant context type info at both the
// beginning and the end.
// CHECK-APPKIT: func reversing() -> NSBezierPath

// Make sure we're dealing with 'instancetype' properly.
// CHECK-APPKIT: func inventing() -> Self

// Make sure we're removing redundant context type info at both the
// beginning and the end of a property.
// CHECK-APPKIT: var flattening: NSBezierPath { get }

// CHECK-APPKIT: func dismissAnimated(_: Bool)

// CHECK-APPKIT: func shouldCollapseAutoExpandedItemsForDeposited(_: Bool) -> Bool

// Introducing argument labels and pruning the base name.
// CHECK-APPKIT: func rectForCancelButtonWhenCentered(_: Bool)

// CHECK-APPKIT: func openUntitledDocumentAndDisplay(_: Bool)

// Don't strip due to weak type information.
// CHECK-APPKIT: func setContentHuggingPriority(_: NSLayoutPriority)

// Look through typedefs of pointers.
// CHECK-APPKIT: func layoutAt(_: NSPointPointer)

// The presence of a property prevents us from stripping redundant
// type information from the base name.
// CHECK-APPKIT: func addGestureRecognizer(_: NSGestureRecognizer)
// CHECK-APPKIT: func removeGestureRecognizer(_: NSGestureRecognizer)
// CHECK-APPKIT: func favoriteViewFor(_: NSGestureRecognizer) -> NSView?
// CHECK-APPKIT: func addLayoutConstraints(_: Set<NSLayoutConstraint>)
// CHECK-APPKIT: func add(_: NSRect)
// CHECK-APPKIT: class func conjureRect(_: NSRect)
