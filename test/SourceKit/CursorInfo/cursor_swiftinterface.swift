import SomeModule

func test() {
  print(SomeFunc())
  let wrapper = XWrapper(x: 43)
  print(wrapper.x)
}

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
//
// Tests CursorInfo works with modules imported via their .swiftinterface file.
// Setup builds a module interface (.swiftinterface) and doc file (.swiftdoc),
// for SomeModule (built from the SomeModule.swift input) that is imported in
// this file. No .swiftmodule is produced to force it into loading via the
// .swiftinterface file.

// Setup phase 1: build the module interface (.swiftinterface) and doc (.swiftdoc).
//
// RUN: %target-swift-frontend -I %t -module-name SomeModule -emit-module-interface-path %t/SomeModule.swiftinterface -emit-module-doc-path %t/SomeModule.swiftdoc %S/Inputs/SomeModule.swift -emit-module -o /dev/null

// Actual test: Check the CusorInfo results of references to symbols in that
// module, including the available refactoring actions, and associated doc
// comments (from the .swiftdoc file).
//
// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=1:8 %s -- -I %t -module-cache-path %t/modulecache -target %target-triple %s | %FileCheck %s -check-prefix=CHECK1
// CHECK1: source.lang.swift.ref.module
// CHECK1: SomeModule
// CHECK1: ACTIONS BEGIN
// CHECK1-NEXT: ACTIONS END

// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=4:9 %s -- -I %t -module-cache-path %t/modulecache -target %target-triple %s | %FileCheck %s -check-prefix=CHECK2
// CHECK2: source.lang.swift.ref.function.free
// CHECK2: SomeFunc()
// CHECK2: () -> Int
// CHECK2: SomeModule
// CHECK2: <CommentParts><Abstract><Para>This is SomeFunc that serves some function</Para></Abstract><ResultDiscussion><Para>42</Para></ResultDiscussion></CommentParts>
// CHECK2: ACTIONS BEGIN
// CHECK2-NEXT: source.refactoring.kind.rename.global
// CHECK2-NEXT: Global Rename
// CHECK2-NEXT: symbol without a declaration location cannot be renamed
// CHECK2-NEXT: ACTIONS END

// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=5:17 %s -- -I %t -module-cache-path %t/modulecache -target %target-triple %s | %FileCheck %s -check-prefix=CHECK3
// CHECK3: source.lang.swift.ref.struct
// CHECK3: XWrapper
// CHECK3: XWrapper.Type
// CHECK3: SomeModule
// CHECK3: ACTIONS BEGIN
// CHECK3-NEXT: source.refactoring.kind.rename.global
// CHECK3-NEXT: Global Rename
// CHECK3-NEXT: symbol without a declaration location cannot be renamed
// CHECK3-NEXT: ACTIONS END

// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=6:9 %s -- -I %t -module-cache-path %t/modulecache -target %target-triple %s | %FileCheck %s -check-prefix=CHECK4
// CHECK4: source.lang.swift.ref.var.local
// CHECK4: wrapper
// CHECK4: XWrapper
// CHECK4: ACTIONS BEGIN
// CHECK4-NEXT: source.refactoring.kind.rename.local
// CHECK4-NEXT: Local Rename
// CHECK4-NEXT: ACTIONS END

// RUN: %sourcekitd-test -req=cursor -cursor-action -pos=6:17 %s -- -I %t -module-cache-path %t/modulecache -target %target-triple %s | %FileCheck %s -check-prefix=CHECK5
// CHECK5: source.lang.swift.ref.var.instance
// CHECK5: x
// CHECK5: Int
// CHECK5: SomeModule
// CHECK5: <CommentParts><Abstract><Para>This is x, a property of XWrapper</Para></Abstract></CommentParts>
// CHECK5: ACTIONS BEGIN
// CHECK5-NEXT: source.refactoring.kind.rename.global
// CHECK5-NEXT: Global Rename
// CHECK5-NEXT: symbol without a declaration location cannot be renamed
// CHECK5-NEXT: ACTIONS END
