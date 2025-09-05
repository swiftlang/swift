// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %target-swift-ide-test -print-module -module-to-print=CountedByNoEscapeClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes -Xcc -Werror -Xcc -Wno-ignored-attributes -Xcc -Wno-nullability-completeness | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedByNoEscape.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-ignored-attributes -Xcc -Wno-nullability-completeness %s

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by __noescape parameters.

import CountedByNoEscapeClang

// CHECK:      /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(_anonymous_param1: copy _anonymous_param1)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func anonymous(_ _anonymous_param1: inout MutableSpan<Int32>?)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(func: copy func)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func clash(func: inout MutableSpan<Int32>?, clash where: Int32)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(_clash2_param1: copy _clash2_param1)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func clash2(func _clash2_param1: inout MutableSpan<Int32>?, clash2 _clash2_param2: Int32)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(_func_param1: copy _func_param1)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func `func`(_ _func_param1: inout MutableSpan<Int32>?)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(func: copy func)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcRenamed(func: inout MutableSpan<Int32>?, extension: Int32, init: Int32, open: Int32, var: Int32, is: Int32, as: Int32, in: Int32, guard: Int32, where: Int32) -> UnsafeMutableRawPointer!

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(_funcRenamedAnon_param1: copy _funcRenamedAnon_param1)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcRenamedAnon(func _funcRenamedAnon_param1: inout MutableSpan<Int32>?, extension _funcRenamedAnon_param2: Int32, init _funcRenamedAnon_param3: Int32, open _funcRenamedAnon_param4: Int32, var _funcRenamedAnon_param5: Int32, is _funcRenamedAnon_param6: Int32, as _funcRenamedAnon_param7: Int32, in _funcRenamedAnon_param8: Int32, guard _funcRenamedAnon_param9: Int32, where _funcRenamedAnon_param10: Int32) -> UnsafeMutableRawPointer!

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(_in_param1: copy _in_param1)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func `in`(func _in_param1: inout MutableSpan<Int32>?, in _in_param2: Int32)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(func: copy func)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func keyword(_ func: inout MutableSpan<Int32>?, _ extension: Int32, _ init: Int32, _ open: Int32, _ var: Int32, _ is: Int32, _ as: Int32, _ in: Int32, _ guard: Int32, _ where: Int32)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func keywordType(_ p: inout MutableSpan<actor?>, _ p2: actor) -> actor

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(_lenName_param2: copy _lenName_param2)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func lenName(_ _lenName_param0: Int32, _ _lenName_param1: Int32, _ _lenName_param2: inout MutableSpan<Int32>?)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: inout MutableSpan<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ p: inout MutableSpan<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ p: inout MutableSpan<Int32>?)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(func: copy func)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func open(func: inout MutableSpan<Int32>?, open where: Int32)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(_pointerName_param1: copy _pointerName_param1)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func pointerName(_ _pointerName_param1: inout MutableSpan<Int32>?)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy p)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnLifetimeBound(_ len1: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32>

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p1: copy p1)
// CHECK-NEXT: @_lifetime(p2: copy p2)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ p: inout MutableSpan<Int32>)

// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_ p: inout MutableSpan<Int32>)

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callComplexExpr(_ p: inout MutableSpan<CInt>) {
  complexExpr(CInt(p.count), 1, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callNonnull(_ p: inout MutableSpan<CInt>) {
  nonnull(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callNullUnspecified(_ p: inout MutableSpan<CInt>) {
  nullUnspecified(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callNullable(_ p: inout MutableSpan<CInt>?) {
  nullable(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callReturnLifetimeBound(_ p: inout MutableSpan<CInt>) {
  let _: MutableSpan<CInt> = returnLifetimeBound(2, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@inlinable
public func callReturnPointer() {
  let _: UnsafeMutableBufferPointer<CInt>? = unsafe returnPointer(4) // call wrapper
  let _: UnsafeMutablePointer<CInt>? = unsafe returnPointer(4) // call unsafe interop
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_lifetime(p2: copy p2)
@inlinable
public func callShared(_ p: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>) {
  shared(&p, &p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callSimple(_ p: inout MutableSpan<CInt>) {
  simple(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callSwiftAttr(_ p: inout MutableSpan<CInt>) {
  swiftAttr(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callAnonymous(_ p: inout MutableSpan<CInt>?) {
  anonymous(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callKeyword(_ p: inout MutableSpan<CInt>?) {
  keyword(&p, 1, 2, 3, 4, 5, 6, 7, 8, 9)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callFunc(_ p: inout MutableSpan<CInt>?) {
  `func`(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callFuncRenameKeyword(_ p: inout MutableSpan<CInt>?) {
  let _ = unsafe funcRenamed(func: &p, extension: 1, init: 2, open: 3, var: 4, is: 5, as: 6, in: 7, guard: 8, where: 9)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callFuncRenameClash(_ p: inout MutableSpan<CInt>?) {
  clash(func: &p, clash: 1)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callFuncRenameClashKeyword(_ p: inout MutableSpan<CInt>?) {
  `open`(func: &p, open: 1)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callFuncRenameClashKeywordAnon(_ p: inout MutableSpan<CInt>?) {
  `in`(func: &p, in: 1)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callPointerName(_ p: inout MutableSpan<CInt>?) {
  pointerName(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@inlinable
public func callLenName(_ p: inout MutableSpan<CInt>?) {
  lenName(CInt(p?.count ?? 0), 2, &p)
}
