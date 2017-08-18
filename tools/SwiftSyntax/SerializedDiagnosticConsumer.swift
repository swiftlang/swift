//===---------- BitstreamWriter.swift - LLVM Bitstream Writer -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file provides an implementation of the Clang diagnostic serialization
// format for diagnostics that have been added to a DiagnosticEngine.
//===----------------------------------------------------------------------===//

import Foundation

class SerializedDiagnosticConsumer: DiagnosticConsumer {
  enum BlockID: UInt8 {
    // must match llvm::bitc::FIRST_APPLICATION_BLOCKID
    case metadata = 8

    case diagnostics

    var name: String {
      switch self {
      case .metadata: return "Meta"
      case .diagnostics: return "Diag"
      }
    }
  }

  enum RecordID: UInt8 {
    case version = 1
    case diagnostic
    case sourceRange
    case diagnosticFlag
    case category
    case filename
    case fixIt

    var name: String {
      switch self {
      case .version: return "Version"
      case .diagnostic: return "DiagInfo"
      case .sourceRange: return "SrcRange"
      case .diagnosticFlag: return "DiagFlag"
      case .category: return "CatName"
      case .filename: return "FileName"
      case .fixIt: return "FixIt"
      }
    }
  }

  let stream = BitstreamWriter()

  /// The URL to write the serialized diagnostics to.
  let outputURL: URL

  /// A mapping from record IDs to their abbreviation IDs
  var abbreviations = [RecordID: AbbreviationID]()

  /// Maps filenames to their record IDs.
  var fileRecords = [String: UInt]()

  static let clangDiagnosticVersion: UInt = 1

  /// Creates a new SerializedDiagnosticConsumer that outputs the diagnostics
  /// to the provided URL.
  init(outputURL: URL) {
    self.outputURL = outputURL
    emitPreamble()
  }

  func emitPreamble() {
    stream.writeASCII("D")
    stream.writeASCII("I")
    stream.writeASCII("A")
    stream.writeASCII("G")
    emitBlockInfoBlock()
    emitMetaBlock()
  }

  func emitBlockID(_ id: BlockID) {
    var idRecord = BitstreamRecord()
    idRecord.append(id)
    stream.writeRecord(BitstreamWriter.BlockInfoCode.setBID, idRecord)

    var nameRecord = BitstreamRecord()
    nameRecord.append(id.name)
    stream.writeRecord(BitstreamWriter.BlockInfoCode.blockName, nameRecord)
  }

  func emitRecordID(_ id: RecordID) {
    var record = BitstreamRecord()
    record.append(id)
    record.append(id.name)
    stream.writeRecord(BitstreamWriter.BlockInfoCode.setRecordName, record)
  }

  /// Gets the file ID associated with the provided filename, or adds it to the
  /// file mapping with a new ID.
  func getOrEmitFile(_ filename: String) -> UInt {
    if let id = fileRecords[filename] { return id }

    // IDs start at 1. 0 means "null file".
    let id = UInt(fileRecords.count + 1)
    fileRecords[filename] = id

    var record = BitstreamRecord()
    record.append(RecordID.filename)
    record.append(id)
    record.append(0 as UInt) // For legacy (file size)
    record.append(0 as UInt) // For legacy (modification time)
    record.append(UInt(filename.utf8.count))
    stream.writeRecord(abbreviations[.filename]!, record, blob: filename)
    return id
  }

  /// Adds a Source Location to the currently-building Bitstream record.
  func addSourceLocation(_ sourceLocation: SourceLocation?,
                         to record: inout BitstreamRecord) {
    guard let loc = sourceLocation else {
      record.append(0 as UInt) // File
      record.append(0 as UInt) // Line
      record.append(0 as UInt) // Column
      record.append(0 as UInt) // Offset
      return
    }

    record.append(getOrEmitFile(loc.file))
    record.append(UInt(loc.line))
    record.append(UInt(loc.column))
    record.append(UInt(loc.offset))
  }

  func addRange(_ sourceRange: SourceRange, to record: inout BitstreamRecord) {
    addSourceLocation(sourceRange.start, to: &record)
    addSourceLocation(sourceRange.end, to: &record)
  }

  func emitDiagnostic(_ diagnostic: Diagnostic) {
    var record = BitstreamRecord()
    record.append(RecordID.diagnostic)
    record.append(diagnostic.kind.rawValue)
    addSourceLocation(diagnostic.location, to: &record)

    record.append(0 as UInt) // Swift diagnostics have no category
    record.append(0 as UInt) // Swift diagnostics have no flags

    record.append(UInt(diagnostic.message.utf8.count))
    stream.writeRecord(abbreviations[.diagnostic]!, record,
                       blob: diagnostic.message)

    // If there's no location, don't emit source ranges or fix-its.
    if diagnostic.location == nil { return }

    // Emit source ranges
    let rangeAbbrev = abbreviations[.sourceRange]!
    for range in diagnostic.highlights {
      var record = BitstreamRecord()
      record.append(RecordID.sourceRange)
      addRange(range, to: &record)
      stream.writeRecord(rangeAbbrev, record)
    }

    // Emit fixIts
    let fixItAbbrev = abbreviations[.fixIt]!
    for fixIt in diagnostic.fixIts {
      var record = BitstreamRecord()
      record.append(RecordID.fixIt)
      addRange(fixIt.range, to: &record)
      record.append(UInt(fixIt.text.utf8.count))
      stream.writeRecord(fixItAbbrev, record, blob: fixIt.text)
    }
  }

  func emitBlockInfoBlock() {
    stream.writeBlockInfoBlock {
      // The subsequent records and Abbrevs are for the "Meta" block.
      emitBlockID(.metadata)
      emitRecordID(.version)

      abbreviations[.version] = stream.defineBlockInfoAbbrev(BlockID.metadata, [
        .literalCode(RecordID.version),
        .fixed(bitWidth: 32)
      ])

      let sourceLocationAbbrev: BitCodeAbbrev = [
        .fixed(bitWidth: 10), // File ID
        .fixed(bitWidth: 32), // Line
        .fixed(bitWidth: 32), // Column
        .fixed(bitWidth: 32), // Offset
      ]

      emitBlockID(.diagnostics)
      emitRecordID(.diagnostic)
      emitRecordID(.sourceRange)
      emitRecordID(.category)
      emitRecordID(.diagnosticFlag)
      emitRecordID(.filename)
      emitRecordID(.fixIt)

      abbreviations[.diagnostic] =
        stream.defineBlockInfoAbbrev(BlockID.diagnostics, [
          .literalCode(RecordID.diagnostic),
          .fixed(bitWidth: 3)   // Diag level.
        ] + sourceLocationAbbrev + [
          .fixed(bitWidth: 10), // Category.
          .fixed(bitWidth: 10), // Mapped Diag ID.
          .fixed(bitWidth: 16), // Text size.
          .blob                 // Diagnostic text.
        ])

      abbreviations[.category] =
        stream.defineBlockInfoAbbrev(BlockID.diagnostics, [
          .literalCode(RecordID.category),
          .fixed(bitWidth: 16), // Category ID
          .fixed(bitWidth: 8),  // Text size
          .blob                 // Category text
        ])

      var sourceRangeAbbrev: BitCodeAbbrev = [.literalCode(RecordID.sourceRange)]
      sourceRangeAbbrev.append(contentsOf: sourceLocationAbbrev)
      sourceRangeAbbrev.append(contentsOf: sourceLocationAbbrev)
      abbreviations[.sourceRange] =
        stream.defineBlockInfoAbbrev(BlockID.diagnostics, sourceRangeAbbrev)

      abbreviations[.diagnosticFlag] =
        stream.defineBlockInfoAbbrev(BlockID.diagnostics, [
          .literalCode(RecordID.diagnosticFlag),
          .fixed(bitWidth: 10), // Mapped Diag ID
          .fixed(bitWidth: 16), // Text size
          .blob                 // Flag name text
        ])

      abbreviations[.filename] =
        stream.defineBlockInfoAbbrev(BlockID.diagnostics, [
          .literalCode(RecordID.filename),
          .fixed(bitWidth: 10), // Mapped File ID
          .fixed(bitWidth: 32), // Size
          .fixed(bitWidth: 32), // Modification time.
          .fixed(bitWidth: 16), // Text size.
          .blob                 // File name text
        ])

      var fixitAbbrev: BitCodeAbbrev = [.literalCode(RecordID.fixIt)]
      fixitAbbrev.append(contentsOf: sourceLocationAbbrev)
      fixitAbbrev.append(contentsOf: sourceLocationAbbrev)
      fixitAbbrev.append(.fixed(bitWidth: 16)) // Text size
      fixitAbbrev.append(.blob)                // FixIt Text
      abbreviations[.fixIt] =
        stream.defineBlockInfoAbbrev(BlockID.diagnostics, fixitAbbrev)
    }
  }

  func emitMetaBlock() {
    stream.writeBlock(BlockID.metadata, newAbbrevWidth: 3) {
      var record = BitstreamRecord()
      record.append(RecordID.version)
      record.append(SerializedDiagnosticConsumer.clangDiagnosticVersion)
      stream.writeRecord(abbreviations[.version]!, record)
    }
  }

  func emitKind(_ kind: Diagnostic.Kind) {
    stream.writeCode(kind)
  }

  func handle(_ diagnostic: Diagnostic) {
    stream.writeBlock(BlockID.diagnostics, newAbbrevWidth: 4) {
      emitDiagnostic(diagnostic)
      for note in diagnostic.notes {
        stream.writeBlock(BlockID.diagnostics, newAbbrevWidth: 4) {
          emitDiagnostic(note.asDiagnostic())
        }
      }
    }
  }

  func finalize() {
    do {
      try Data(stream.data).write(to: outputURL)
    } catch {
      let diagEngine = DiagnosticEngine()
      let printingConsumer = PrintingDiagnosticConsumer()
      diagEngine.addConsumer(printingConsumer)
      diagEngine.diagnose(.error("could not write file '\(outputURL.path)'",
                                 location: nil))
    }
  }
}
