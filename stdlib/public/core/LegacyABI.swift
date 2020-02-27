//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file contains non-API (or underscored) declarations that are needed to
// be kept around for ABI compatibility

extension Unicode.UTF16 {
  @available(*, unavailable, renamed: "Unicode.UTF16.isASCII")
  @inlinable
  public static func _isASCII(_ x: CodeUnit) -> Bool  {
    return Unicode.UTF16.isASCII(x)
  }
}

@available(*, unavailable, renamed: "Unicode.UTF8.isASCII")
@inlinable
internal func _isASCII(_ x: UInt8) -> Bool {
  return Unicode.UTF8.isASCII(x)
}

@available(*, unavailable, renamed: "Unicode.UTF8.isContinuation")
@inlinable
internal func _isContinuation(_ x: UInt8) -> Bool {
  return UTF8.isContinuation(x)
}

extension Substring {
@available(*, unavailable, renamed: "Substring.base")
  @inlinable
  internal var _wholeString: String { return base }
}

extension String {
  @available(*, unavailable, renamed: "String.withUTF8")
  @inlinable
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    var copy = self
    return try copy.withUTF8(body)
  }
}

extension Substring {
  @available(*, unavailable, renamed: "Substring.withUTF8")
  @inlinable
  internal func _withUTF8<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    var copy = self
    return try copy.withUTF8(body)
  }
}

// This function is no longer used but must be kept for ABI compatibility
// because references to it may have been inlined.
@usableFromInline
internal func _branchHint(_ actual: Bool, expected: Bool) -> Bool {
  // The LLVM intrinsic underlying int_expect_Int1 now requires an immediate
  // argument for the expected value so we cannot call it here. This should
  // never be called in cases where performance matters, so just return the
  // value without any branch hint.
  return actual
}

extension String {
  @usableFromInline // Never actually used in inlinable code...
  internal func _nativeCopyUTF16CodeUnits(
    into buffer: UnsafeMutableBufferPointer<UInt16>,
    range: Range<String.Index>
  ) { fatalError() }
}

extension String.UTF16View {
  // Swift 5.x: This was accidentally shipped as inlinable, but was never used
  // from an inlinable context. The definition is kept around for techincal ABI
  // compatibility (even though it shouldn't matter), but is unused.
  @inlinable @inline(__always)
  internal var _shortHeuristic: Int { return 32 }
}

// Legacy Integer Parsing
// The functions below need to be preserved for ABI compatibility, since
// references to them might have been inlined. Note that in most cases they
// themselves will likely have been inlined as well since they were all
// originally marked @inline(__always).

@usableFromInline
internal func _ascii16(_ c: Unicode.Scalar) -> UTF16.CodeUnit {
  return UTF16.CodeUnit(c.value)
}

@usableFromInline
internal func _asciiDigit<CodeUnit: UnsignedInteger, Result: BinaryInteger>(
  codeUnit u_: CodeUnit, radix: Result
) -> Result? {
  let digit = _ascii16("0")..._ascii16("9")
  let lower = _ascii16("a")..._ascii16("z")
  let upper = _ascii16("A")..._ascii16("Z")

  let u = UInt16(truncatingIfNeeded: u_)
  let d: UInt16
  if _fastPath(digit ~= u) { d = u &- digit.lowerBound }
  else if _fastPath(upper ~= u) { d = u &- upper.lowerBound &+ 10 }
  else if _fastPath(lower ~= u) { d = u &- lower.lowerBound &+ 10 }
  else { return nil }
  guard _fastPath(d < radix) else { return nil }
  return Result(truncatingIfNeeded: d)
}

@usableFromInline
internal func _parseUnsignedASCII<
  Rest: IteratorProtocol, Result: FixedWidthInteger
>(
  first: Rest.Element, rest: inout Rest, radix: Result, positive: Bool
) -> Result?
where Rest.Element: UnsignedInteger {
  let r0 = _asciiDigit(codeUnit: first, radix: radix)
  guard _fastPath(r0 != nil), var result = r0 else { return nil }
  if !positive {
    let (result0, overflow0)
      = (0 as Result).subtractingReportingOverflow(result)
    guard _fastPath(!overflow0) else { return nil }
    result = result0
  }

  while let u = rest.next() {
    let d0 = _asciiDigit(codeUnit: u, radix: radix)
    guard _fastPath(d0 != nil), let d = d0 else { return nil }
    let (result1, overflow1) = result.multipliedReportingOverflow(by: radix)
    let (result2, overflow2) = positive
      ? result1.addingReportingOverflow(d)
      : result1.subtractingReportingOverflow(d)
    guard _fastPath(!overflow1 && !overflow2)
    else { return nil }
    result = result2
  }
  return result
}

@usableFromInline
internal func _parseASCII<
  CodeUnits: IteratorProtocol, Result: FixedWidthInteger
>(
  codeUnits: inout CodeUnits, radix: Result
) -> Result?
where CodeUnits.Element: UnsignedInteger {
  let c0_ = codeUnits.next()
  guard _fastPath(c0_ != nil), let c0 = c0_ else { return nil }
  if _fastPath(c0 != _ascii16("+") && c0 != _ascii16("-")) {
    return _parseUnsignedASCII(
      first: c0, rest: &codeUnits, radix: radix, positive: true)
  }
  let c1_ = codeUnits.next()
  guard _fastPath(c1_ != nil), let c1 = c1_ else { return nil }
  if _fastPath(c0 == _ascii16("-")) {
    return _parseUnsignedASCII(
      first: c1, rest: &codeUnits, radix: radix, positive: false)
  }
  else {
    return _parseUnsignedASCII(
      first: c1, rest: &codeUnits, radix: radix, positive: true)
  }
}

extension FixedWidthInteger {
  @usableFromInline
  internal static func _parseASCIISlowPath<
    CodeUnits: IteratorProtocol, Result: FixedWidthInteger
  >(codeUnits: inout CodeUnits, radix: Result) -> Result?
  where CodeUnits.Element: UnsignedInteger {
    return _parseASCII(codeUnits: &codeUnits, radix: radix)
  }
}
