//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(Compiler) import SwiftIDEUtils
import SwiftSyntax
import swiftASTGen
import IDEBridging

// MARK: - SourceLocVector Sequence conformance

#if swift(<5.9)

/// Needed because C++ interop does not automatically conform SourceLocVector to CxxSequence with a Swift 5.8 compiler.
extension SourceLocVectorIterator: UnsafeCxxInputIterator {
   public static func ==(lhs: SourceLocVectorIterator, rhs: SourceLocVectorIterator) -> Bool { Self.equals(lhs, rhs) }
}
extension SourceLocVector: CxxSequence {
  public typealias RawIterator = SourceLocVectorIterator
}
#endif

// MARK: - Bridged type initializers

fileprivate extension BridgedCharSourceRange {
  init(from range: Range<AbsolutePosition>, in sourceFile: ExportedSourceFile) {
    self.init(
      start: BridgedSourceLoc(at: range.lowerBound, in: sourceFile.buffer),
      byteLength: UInt32(range.upperBound.utf8Offset - range.lowerBound.utf8Offset)
    )
  }
}

fileprivate extension CharSourceRangeVector {
  init(from ranges: some Sequence<Range<AbsolutePosition>>, in sourceFile: ExportedSourceFile) {
    self = .empty()
    for range in ranges {
      self.push_back(BridgedCharSourceRange(from: range, in: sourceFile))
    }
  }
}

fileprivate extension ResolvedLocVector {
  init(from resolvedLocs: some Sequence<DeclNameLocation>, in sourceFile: ExportedSourceFile) {
    self = .empty()
    for resolvedLoc in resolvedLocs {
      self.push_back(IDEBridging.ResolvedLoc(from: resolvedLoc, in: sourceFile))
    }
  }
}

fileprivate extension IDEBridging.LabelRangeType {
  init(_ argumentType: DeclNameLocation.Arguments) {
    switch argumentType {
    case .noArguments: self = .None
    case .call: self = .CallArg
    case .parameters: self = .Param
    case .noncollapsibleParameters: self = .NoncollapsibleParam
    case .selector: self = .Selector
    }
  }
}

extension IDEBridging.ResolvedLoc {
  init(from resolvedLoc: DeclNameLocation, in sourceFile: ExportedSourceFile) {
    let firstTrailingClosureIndex: UInt32
    if case .call(_, .some(let index)) = resolvedLoc.arguments {
      firstTrailingClosureIndex = UInt32(index)
    } else {
      firstTrailingClosureIndex = UInt32.max
    }
    let arguments: [DeclNameLocation.Argument]
    switch resolvedLoc.arguments {
      case .noArguments: arguments = []
      case .call(let arguments2, _): arguments = arguments2
      case .parameters(let arguments2): arguments = arguments2
      case .noncollapsibleParameters(let arguments2): arguments = arguments2
      case .selector(let arguments2): arguments = arguments2
    }
    self.init(
      range: BridgedCharSourceRange(from: resolvedLoc.baseNameRange, in: sourceFile), 
      labelRanges: CharSourceRangeVector(from: arguments.map { $0.range }, in: sourceFile), 
      firstTrailingLabel: firstTrailingClosureIndex,
      labelType: LabelRangeType(resolvedLoc.arguments), 
      isActive: resolvedLoc.isActive, 
      context: IDEBridging.ResolvedLocContext(resolvedLoc.context)
    )
  }
}

fileprivate extension IDEBridging.ResolvedLocContext {
  init(_ context: DeclNameLocation.Context) {
    switch context {
    case .default: self = .Default
    case .selector: self = .Selector
    case .comment: self = .Comment
    case .stringLiteral: self = .StringLiteral
    }
  }
}

// MARK: - Run NameMatcher

public func runNameMatcher(
  sourceFilePtr: UnsafeRawPointer,
  locations: SourceLocVector
) -> ResolvedLocVector {
  let sourceFile = sourceFilePtr.bindMemory(to: ExportedSourceFile.self, capacity: 1).pointee
  let positions: [AbsolutePosition] = locations.compactMap { sourceFile.position(of: $0) }
  let resolvedLocs = NameMatcher.resolve(baseNamePositions: positions, in: sourceFile.syntax)
  return ResolvedLocVector(from: resolvedLocs, in: sourceFile)
}
