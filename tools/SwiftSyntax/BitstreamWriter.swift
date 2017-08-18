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
// BitstreamWriter provides functions for writing LLVM Bitstream-encoded data to
// a data buffer.
//===----------------------------------------------------------------------===//

import Foundation

typealias UnsignedIntegralType = UnsignedInteger & ExpressibleByIntegerLiteral

struct BitstreamRecord {
  private(set) var values = [UInt32]()

  mutating func append<IntType: UnsignedIntegralType>(_ int: IntType) {
    values.append(UInt32(int))
  }

  mutating func append<CodeType: RawRepresentable>(_ code: CodeType)
    where CodeType.RawValue == UInt8 {
    values.append(UInt32(code.rawValue))
  }

  mutating func append(_ string: String) {
    for byte in string.utf8 {
      values.append(UInt32(byte))
    }
  }
}

typealias AbbreviationID = UInt

class BitstreamWriter {
  enum FixedAbbrevID: UInt8 {
    case endBlock = 0
    case enterSubblock = 1
    case defineAbbrev = 2
    case unabbrevRecord = 3
  }
  enum BlockID: UInt8 {
    case blockInfo = 0
  }
  enum BlockInfoCode: UInt8 {
    case setBID = 1
    case blockName = 2
    case setRecordName = 3
  }

  /// The buffer of data being written to.
  private(set) var data = [UInt8]()

  /// The current value. Only bits < currentBit are valid.
  private var currentValue: UInt32 = 0

  /// Always between 0 and 31 inclusive, specifies the next bit to use.
  private var currentBit: UInt8 = 0

  /// The bit width used for abbreviated codes.
  private var codeBitWidth: UInt8

  /// The list of defined abbreviations.
  private var currentAbbreviations = [BitCodeAbbrev]()

  /// Represents an in-flight block currently being emitted.
  struct Block {
    /// The code width before we started emitting this block.
    let previousCodeWidth: UInt8

    /// The index into the data buffer where this block's length placeholder
    /// lives.
    let lengthPlaceholderByteIndex: Int

    /// The previous set of abbreviations registered.
    let previousAbbrevs: [BitCodeAbbrev]
  }

  /// This keeps track of the blocks that are being emitted.
  private var blockScope = [Block]()

  /// This contains information emitted to BLOCKINFO_BLOCK blocks.
  /// These describe abbreviations that all blocks of the specified ID inherit.
  class BlockInfo {
    let id: UInt8
    var abbrevs = [BitCodeAbbrev]()

    /// Constructs a BlockInfo with the provided ID.
    init(id: UInt8) {
      self.id = id
    }
  }
  /// This maps BlockInfo IDs to their corresponding values.
  private var blockInfoRecords = [UInt8: BlockInfo]()

  /// When emitting blockinfo, this is the ID of the current block being
  /// emitted.
  private var currentBlockID: UInt8?

  /// Gets the BlockInfo for the provided ID or creates it if it hasn't been
  /// created already.
  func getOrCreateBlockInfo(_ id: UInt8) -> BlockInfo {
    if let blockInfo = blockInfoRecords[id] { return blockInfo }
    let info = BlockInfo(id: id)
    blockInfoRecords[id] = info
    return info
  }

  /// Creates a new BitstreamWriter with the provided bit width for coded IDs.
  init(codeBitWidth: UInt8 = 2) {
    self.codeBitWidth = codeBitWidth
  }

  func writeBytes(_ int: UInt32, byteIndex: Int) {
    let i = int.littleEndian
    data.withUnsafeMutableBytes { ptr in
      ptr.storeBytes(of: i, toByteOffset: byteIndex, as: UInt32.self)
    }
  }

  /// Writes the provided UInt32 to the data stream directly.
  func write(_ int: UInt32) {
    let index = data.count

    // Add 4 bytes of zeroes to be overwritten.
    data.append(0)
    data.append(0)
    data.append(0)
    data.append(0)

    writeBytes(int, byteIndex: index)
  }

  /// Writes the provided number of bits to the buffer.
  ///
  /// - Parameters:
  ///   - int: The integer containing the bits you'd like to write
  ///   - width: The number of low-bits of the integer you're writing to the
  ///            buffer
  func writeVBR<IntType: UnsignedIntegralType>(_ int: IntType, width: UInt8) {
    let threshold = UInt64(1) << (UInt64(width) - 1)
    var value = UInt64(int)

    // Emit the bits with VBR encoding, (width - 1) bits at a time.
    while value >= threshold {
      let masked = (value & (threshold - 1)) | threshold
      write(masked, width: width)
      value >>= width - 1
    }

    write(value, width: width)
  }

  /// Writes the provided number of bits to the buffer.
  ///
  /// - Parameters:
  ///   - int: The integer containing the bits you'd like to write
  ///   - width: The number of low-bits of the integer you're writing to the
  ///            buffer
  func write<IntType: UnsignedIntegralType>(_ int: IntType, width: UInt8) {
    precondition(width > 0, "cannot emit 0 bits")
    precondition(width <= 32, "can only write at most 32 bits")

    let intPattern = UInt32(int)

    precondition(intPattern & ~(~(0 as UInt32) >> (32 - width)) == 0,
                 "High bits set!")

    // Mask the bits of the argument over the current bit we're tracking
    let intMask = intPattern << currentBit
    currentValue |= intMask

    // If we haven't spilled past the temp buffer, just update the current bit.
    if currentBit + width < 32 {
      currentBit += width
      return
    }

    // Otherwise, write the current value.
    write(currentValue)

    if currentBit > 0 {
      // If we still have bits leftover, replace the current buffer with the
      // low bits of the input, offset by the current bit.
      // For example, when we're adding:
      // 0b00000000_00000000_00000000_00000011
      // to
      // 0b01111111_11111111_11111111_11111111
      //    ^ currentBit (31)
      // We've already taken 1 bit off the end of the first number, leaving an
      // extra 1 bit that needs to be represented for the next write.
      // Subtract the currentBit from 32 to get the number of bits leftover and
      // then shift to get rid of the already-recorded bits.
      currentValue = UInt32(int) >> (32 - UInt32(currentBit))
    } else {
      // Otherwise, reset our buffer.
      currentValue = 0
    }
    currentBit = (currentBit + width) & 31
  }

  func alignIfNeeded() {
    guard currentBit > 0 else { return }
    write(currentValue)
    assert(bufferOffset % 4 == 0, "buffer must be 32-bit aligned")
    currentValue = 0
    currentBit = 0
  }

  /// Writes a Bool as a 1-bit integer value.
  func write(_ bool: Bool) {
    write(bool ? 1 as UInt : 0, width: 1)
  }

  /// Writes the provided BitCode Abbrev operand to the stream.
  func write(_ abbrevOp: BitCodeAbbrevOp) {
    write(abbrevOp.isLiteral) // the Literal bit.
    switch abbrevOp {
    case .literal(let value):
      // Literal values are 1 (for the Literal bit) and then a vbr8 encoded
      // literal.
      writeVBR(value, width: 8)
    case .fixed(let bitWidth):
      // Fixed values are the encoding kind then the bitWidth as a vbr5 value.
      write(abbrevOp.encodedKind, width: 3)
      writeVBR(bitWidth, width: 5)
    case .vbr(let chunkBitWidth):
      // VBR values are the encoding kind then the chunk width as a vbr5 value.
      write(abbrevOp.encodedKind, width: 3)
      writeVBR(chunkBitWidth, width: 5)
    case .array(let eltOp):
      // Arrays are encoded as the Array kind, then the element type directly
      // after.
      write(abbrevOp.encodedKind, width: 3)
      write(eltOp)
    case .char6, .blob:
      // Blobs and Char6 are just their encoding kind.
      write(abbrevOp.encodedKind, width: 3)
    }
  }

  /// Writes the specified Code value to the stream, as a 32-bit quantity.
  func writeCode<CodeType: RawRepresentable>(_ code: CodeType)
    where CodeType.RawValue == UInt8 {
    writeCode(code.rawValue)
  }

  /// Writes the specified Code value to the stream, as a 32-bit quantity.
  func writeCode<IntType: UnsignedIntegralType>(_ code: IntType) {
    write(code, width: codeBitWidth)
  }

  var bufferOffset: Int {
    return data.count
  }

  /// The current word we're writing to.
  var wordIndex: Int {
    let offset = bufferOffset
    precondition(offset % 3 == 0, "Not 32-bit aligned")
    return offset / 4
  }

  /// \brief Retrieve the current position in the stream, in bits.
  var bitNumber: Int {
    return bufferOffset * 8 + Int(currentBit)
  }

  /// Writes an ASCII character to the stream, as an 8-bit ascii value.
  func writeASCII(_ character: Character) {
    precondition(character.unicodeScalars.count == 1, "character is not ASCII")
    let c = UInt8(ascii: character.unicodeScalars.first!)
    write(c, width: 8)
  }

  /// The first application-defined abbreviation ID.
  static let firstApplicationAbbrevID: UInt = 4

  /// Encodes the definition of an abbreviation to the stream.
  func encodeAbbrev(_ abbrev: BitCodeAbbrev) {
    writeCode(FixedAbbrevID.defineAbbrev)
    writeVBR(UInt(abbrev.count), width: 5)
    for op in abbrev {
      write(op)
    }
  }

  /// Defines an abbreviation and returns the unique identifier for that
  /// abbreviation.
  func defineAbbrev(_ abbrev: BitCodeAbbrev) -> AbbreviationID {
    encodeAbbrev(abbrev)
    currentAbbreviations.append(abbrev)
    return UInt(currentAbbreviations.count - 1) +
      BitstreamWriter.firstApplicationAbbrevID
  }

  /// Char6 is encoded using a special encoding that uses 0 to 64 to encode
  /// English alphanumeric identifiers.
  /// The ranges are specified as:
  /// 'a' .. 'z' ---  0 .. 25
  /// 'A' .. 'Z' --- 26 .. 51
  /// '0' .. '9' --- 52 .. 61
  ///        '.' --- 62
  ///        '_' --- 63
  static let char6Map =
    Array(zip("abcdefghijklmnopqrstuvwxyz" +
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
              "0123456789._", (0 as UInt)...))

  /// Writes a char6-encoded value.
  func writeChar6<IntType: UnsignedIntegralType>(_ value: IntType) {
    guard (0..<64).contains(value) else {
      fatalError("invalid char6 value")
    }
    let v = BitstreamWriter.char6Map[Int(value)].1
    write(v, width: 6)
  }

  /// Writes a value with the provided abbreviation encoding.
  func writeAbbrevField(_ op: BitCodeAbbrevOp, value: UInt32) {
    switch op {
    case .literal(let literalValue):
      // Do not write anything
      precondition(value == literalValue,
                   "literal value must match abbreviated literal " +
                   "(expected \(literalValue), got \(value))")
    case .fixed(let bitWidth):
      write(value, width: bitWidth)
    case .vbr(let chunkBitWidth):
      writeVBR(value, width: chunkBitWidth)
    case .char6:
      writeChar6(value)
    case .blob, .array:
      fatalError("cannot emit a field as array or blob")
    }
  }

  /// Writes an unabbreviated record to the stream.
  func writeRecord<CodeType: RawRepresentable>
    (_ code: CodeType, _ record: BitstreamRecord)
    where CodeType.RawValue == UInt8 {
    writeCode(FixedAbbrevID.unabbrevRecord)
    writeVBR(code.rawValue, width: 6)
    writeVBR(UInt(record.values.count), width: 6)
    for value in record.values {
      writeVBR(value, width: 6)
    }
  }

  /// Writes a record with the provided abbreviation ID and record contents.
  /// Optionally, emits the provided blob if the abbreviation referenced
  /// by that ID requires it.
  func writeRecord(_ abbrevID: AbbreviationID, _ record: BitstreamRecord,
                   blob: String? = nil) {
    let index = abbrevID - BitstreamWriter.firstApplicationAbbrevID
    guard index < currentAbbreviations.count else {
      fatalError("unregistered abbreviation \(index)")
    }

    let abbrev = currentAbbreviations[Int(index)]
    let values = record.values
    var valueIndex = 0
    writeCode(abbrevID)
    for op in abbrev {
      switch op {
      case .array(let eltOp):
        // First, emit the length as a VBR6
        let length = UInt(values.count - valueIndex)
        writeVBR(length, width: 6)

        // Emit the remaining values using that encoding.
        for idx in valueIndex..<values.count {
          writeAbbrevField(eltOp, value: values[idx])
        }
      case .blob:
        guard let blob = blob else { fatalError("expected blob") }
        // Blobs are encoded as a VBR6 length, then a sequence of 8-bit values.
        let length = UInt(blob.utf8.count)
        writeVBR(length, width: 6)
        alignIfNeeded()

        for char in blob.utf8 {
          write(char, width: 8)
        }

        // Ensure total length of the blob is a multiple of 4 by writing zeroes.
        alignIfNeeded()
      default:
        // Otherwise, write this value using its encoding directly and
        // increment the value index.
        writeAbbrevField(op, value: values[valueIndex])
        valueIndex += 1
      }
    }
  }

  /// Writes a block, beginning with the provided block code and the
  /// abbreviation width
  func writeBlock<CodeType: RawRepresentable>
    (_ blockID: CodeType, newAbbrevWidth: UInt8? = nil,
     emitRecords: () -> Void) where CodeType.RawValue == UInt8 {
    enterSubblock(blockID, newAbbrevBitWidth: newAbbrevWidth)
    emitRecords()
    endBlock()
  }

  /// Writes the blockinfo block and allows emitting abbreviations
  /// and records in it.
  func writeBlockInfoBlock(emitRecords: () -> Void) {
    writeBlock(BlockID.blockInfo, newAbbrevWidth: 2) {
      currentBlockID = nil
      blockInfoRecords = [:]
      emitRecords()
    }
  }

  func `switch`(toBlockID blockID: UInt8) {
    if currentBlockID == blockID { return }
    var record = BitstreamRecord()
    record.append(blockID)
    writeRecord(BlockInfoCode.setBID, record)
    currentBlockID = blockID
  }

  func enterSubblock<CodeType: RawRepresentable>
    (_ blockID: CodeType, newAbbrevBitWidth: UInt8? = nil)
    where CodeType.RawValue == UInt8 {
    // [ENTER_SUBBLOCK, blockid(vbr8), newabbrevlen(vbr4),
    //                  <align32bits>, blocklen_32]
    writeCode(FixedAbbrevID.enterSubblock)

    let newWidth = newAbbrevBitWidth ?? codeBitWidth

    writeVBR(blockID.rawValue,  width: 8)

    writeVBR(newWidth, width: 4)
    alignIfNeeded()

    // Caller is responsible for filling in the blocklen_32 value after emitting
    // the contents of the block.
    let byteOffset = bufferOffset
    write(0 as UInt, width: 32)

    let block = Block(previousCodeWidth: codeBitWidth,
                      lengthPlaceholderByteIndex: byteOffset,
                      previousAbbrevs: currentAbbreviations)

    codeBitWidth = newWidth
    currentAbbreviations = []
    blockScope.append(block)
    if let blockInfo = blockInfoRecords[blockID.rawValue] {
      currentAbbreviations.append(contentsOf: blockInfo.abbrevs)
    }
  }

  func endBlock() {
    guard let block = blockScope.popLast() else {
      fatalError("endBlock() called with no block registered")
    }

    let blockLengthInBytes = data.count - block.lengthPlaceholderByteIndex
    let blockLengthIn32BitWords = UInt32(blockLengthInBytes / 4)

    writeCode(FixedAbbrevID.endBlock)
    alignIfNeeded()

    // Backpatch the block length now that we've finished it
    writeBytes(blockLengthIn32BitWords,
               byteIndex: block.lengthPlaceholderByteIndex)

    // Restore the inner block's code size and abbrev table.
    codeBitWidth = block.previousCodeWidth
    currentAbbreviations = block.previousAbbrevs
  }

  /// Defines an abbreviation within the blockinfo block for the provided
  /// block ID.
  func defineBlockInfoAbbrev<CodeType: RawRepresentable>
    (_ blockID: CodeType, _ abbrev: BitCodeAbbrev) -> AbbreviationID
    where CodeType.RawValue == UInt8 {
    self.switch(toBlockID: blockID.rawValue)
    encodeAbbrev(abbrev)
    let info = getOrCreateBlockInfo(blockID.rawValue)
    info.abbrevs.append(abbrev)
    return UInt(info.abbrevs.count - 1) + BitstreamWriter.firstApplicationAbbrevID
  }
}
