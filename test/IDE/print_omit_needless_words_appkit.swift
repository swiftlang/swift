// RUN: %empty-directory(%t)

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/AppKit.swift

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=AppKit -function-definitions=false -prefer-type-repr=true  -skip-parameter-names > %t.AppKit.txt
// RUN: %FileCheck %s -check-prefix=CHECK-APPKIT -strict-whitespace < %t.AppKit.txt

// Note: class method name stripping context type.
// CHECK-APPKIT: class func red() -> NSColor

// Note: instance method name stripping context type.
// CHECK-APPKIT: func same() -> Self

// Note: Unsafe(Mutable)Pointers don't get defaulted to 'nil'
// CHECK-APPKIT: func getRGBAComponents(_: UnsafeMutablePointer<Int8>?)

// Note: Skipping over "3D"
// CHECK-APPKIT: func drawInAir(at: Point3D)

// Note: with<something> -> <something>
// CHECK-APPKIT: func draw(at: Point3D, withAttributes: [String : Any]? = nil)

// Note: Don't strip names that aren't preceded by a verb or preposition.
// CHECK-APPKIT: func setTextColor(_: NSColor?)

// Note: Splitting with default arguments.
// CHECK-APPKIT: func draw(in: NSView?)

// Note: NSDictionary default arguments for "options"
// CHECK-APPKIT: func drawAnywhere(in: NSView?, options: [AnyHashable : Any] = [:])
// CHECK-APPKIT: func drawAnywhere(options: [AnyHashable : Any] = [:])
// CHECK-APPKIT: func drawAnywhere(optionalOptions: [AnyHashable : Any]? = nil)

// Make sure we're removing redundant context type info at both the
// beginning and the end.
// CHECK-APPKIT: func reversing() -> NSBezierPath

// Make sure we're dealing with 'instancetype' properly.
// CHECK-APPKIT: func inventing() -> Self

// Make sure we're removing redundant context type info at both the
// beginning and the end of a property.
// CHECK-APPKIT: var flattened: NSBezierPath { get }

// CHECK-APPKIT: func dismiss(animated: Bool)

// CHECK-APPKIT: func shouldCollapseAutoExpandedItems(forDeposited: Bool) -> Bool

// Introducing argument labels and pruning the base name.
// CHECK-APPKIT: func rectForCancelButton(whenCentered: Bool)

// CHECK-APPKIT: func openUntitledDocumentAndDisplay(_: Bool)

// Don't strip due to weak type information.
// CHECK-APPKIT: func setContentHuggingPriority(_: NSLayoutPriority)

// Look through typedefs of pointers.
// CHECK-APPKIT: func layout(at: NSPointPointer!)

// The presence of a property prevents us from stripping redundant
// type information from the base name.
// CHECK-APPKIT: func addGestureRecognizer(_: NSGestureRecognizer)
// CHECK-APPKIT: func removeGestureRecognizer(_: NSGestureRecognizer)
// CHECK-APPKIT: func favoriteView(for: NSGestureRecognizer) -> NSView?
// CHECK-APPKIT: func addLayoutConstraints(_: Set<NSLayoutConstraint>)
// CHECK-APPKIT: func add(_: NSRect)
// CHECK-APPKIT: class func conjureRect(_: NSRect)
