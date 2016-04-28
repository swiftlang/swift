//===--- SwiftReflectionTest.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides infrastructure for introspecting type information in a
// remote swift executable by swift-reflection-test, using pipes and a
// request-response protocol to communicate with the test tool.
//
//===----------------------------------------------------------------------===//

// FIXME: Make this work with Linux

import MachO
import Darwin

let RequestInstanceAddress = "i"
let RequestReflectionInfos = "r"
let RequestReadBytes = "b";
let RequestSymbolAddress = "s"
let RequestStringLength = "l"
let RequestExit = "e"
let RequestPointerSize = "p"

internal func debugLog(_ message: String) {
#if DEBUG_LOG
  fputs("Child: \(message)\n", stderr)
  fflush(stderr)
#endif
}

/// Represents a section in a loaded image in this process.
internal struct Section {
  /// The absolute start address of the section's data in this address space.
  let startAddress: UnsafePointer<Void>

  /// The size of the section in bytes.
  let size: UInt
}

/// Holds the addresses and sizes of sections related to reflection
internal struct ReflectionInfo : Sequence {
  /// The name of the loaded image
  internal let imageName: String

  /// Reflection metadata sections
  internal let fieldmd: Section?
  internal let assocty: Section?
  internal let builtin: Section?
  internal let typeref: Section?
  internal let reflstr: Section?

  internal func makeIterator() -> AnyIterator<Section?> {
    return AnyIterator([
      fieldmd,
      assocty,
      builtin,
      typeref,
      reflstr
    ].makeIterator())
  }
}

#if arch(x86_64) || arch(arm64)
typealias MachHeader = mach_header_64
#else
typealias MachHeader = mach_header
#endif

/// Get the location and size of a section in a binary.
///
/// - Parameter name: The name of the section
/// - Parameter imageHeader: A pointer to the Mach header describing the
///   image.
/// - Returns: A `Section` containing the address and size, or `nil` if there
///   is no section by the given name.
internal func getSectionInfo(_ name: String,
  _ imageHeader: UnsafePointer<MachHeader>) -> Section? {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  var size: UInt = 0
  let address = getsectiondata(imageHeader, "__DATA", name, &size)
  guard let nonNullAddress = address else { return nil }
  guard size != 0 else { return nil }
  return Section(startAddress: nonNullAddress, size: size)
}

/// Get the Swift Reflection section locations for a loaded image.
///
/// An image of interest must have the following sections in the __DATA
/// segment:
/// - __swift3_fieldmd
/// - __swift3_assocty
/// - __swift3_builtin
/// - __swift3_typeref
/// - __swift3_reflstr (optional, may have been stripped out)
///
/// - Parameter i: The index of the loaded image as reported by Dyld.
/// - Returns: A `ReflectionInfo` containing the locations of all of the
///   needed sections, or `nil` if the image doesn't contain all of them.
internal func getReflectionInfoForImage(atIndex i: UInt32) -> ReflectionInfo? {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  let header = unsafeBitCast(_dyld_get_image_header(i),
    to: UnsafePointer<MachHeader>.self)

  let imageName = _dyld_get_image_name(i)!
  if let fieldmd = getSectionInfo("__swift3_fieldmd", header) {
     let assocty = getSectionInfo("__swift3_assocty", header)
     let builtin = getSectionInfo("__swift3_builtin", header)
     let typeref = getSectionInfo("__swift3_typeref", header)
     let reflstr = getSectionInfo("__swift3_reflstr", header)
      return ReflectionInfo(imageName: String(validatingUTF8: imageName)!,
                            fieldmd: fieldmd,
                            assocty: assocty,
                            builtin: builtin,
                            typeref: typeref,
                            reflstr: reflstr)
  }
  return nil
}

internal func sendBytes<T>(from address: UnsafePointer<T>, count: Int) {
  var source = address
  var bytesLeft = count
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  while bytesLeft > 0 {
    let bytesWritten = fwrite(source, 1, bytesLeft, stdout)
    fflush(stdout)
    guard bytesWritten > 0 else {
      fatalError("Couldn't write to parent pipe")
    }
    bytesLeft -= bytesWritten
    source = source.advanced(by: bytesWritten)
  }
}

/// Send the address of an object to the parent.
internal func sendAddress(of instance: AnyObject) {
  debugLog("BEGIN \(#function)")
  defer { debugLog("END \(#function)") }
  var address = unsafeAddress(of: instance)
  sendBytes(from: &address, count: sizeof(UInt.self))
}

/// Send the `value`'s bits to the parent.
internal func sendValue<T>(_ value: T) {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  var value = value
  sendBytes(from: &value, count: sizeof(T.self))
}

/// Read a word-sized unsigned integer from the parent.
internal func readUInt() -> UInt {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  var value: UInt = 0
  fread(&value, sizeof(UInt.self), 1, stdin)
  return value
}

/// Send all known `ReflectionInfo`s for all images loaded in the current
/// process.
internal func sendReflectionInfos() {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  let infos = (0..<_dyld_image_count()).flatMap(getReflectionInfoForImage)

  var numInfos = infos.count
  debugLog("\(numInfos) reflection info bundles.")
  sendBytes(from: &numInfos, count: sizeof(UInt.self))
  precondition(numInfos >= 1)
  for info in infos {
    debugLog("Sending info for \(info.imageName)")
    let imageNameBytes = Array(info.imageName.utf8)
    var imageNameLength = UInt(imageNameBytes.count)
    fwrite(&imageNameLength, sizeof(UInt.self), 1, stdout)
    fflush(stdout)
    fwrite(imageNameBytes, 1, imageNameBytes.count, stdout)
    fflush(stdout)
    for section in info {
      sendValue(section?.startAddress)
      sendValue(section?.size ?? 0)
    }
  }
}

internal func printErrnoAndExit() {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  let errorCString = strerror(errno)!
  let message = String(validatingUTF8: errorCString)! + "\n"
  let bytes = Array(message.utf8)
  fwrite(bytes, 1, bytes.count, stderr)
  fflush(stderr)
  exit(EXIT_FAILURE)
}

/// Retrieve the address and count from the parent and send the bytes back.
internal func sendBytes() {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  let address = readUInt()
  let count = Int(readUInt())
  debugLog("Parent requested \(count) bytes from \(address)")
  var totalBytesWritten = 0
  var pointer = unsafeBitCast(address, to: UnsafeMutablePointer<Void>.self)
  while totalBytesWritten < count {
    let bytesWritten = Int(fwrite(pointer, 1, Int(count), stdout))
    fflush(stdout)
    if bytesWritten == 0 {
      printErrnoAndExit()
    }
    totalBytesWritten += bytesWritten
    pointer = pointer.advanced(by: bytesWritten)
  }
}

/// Send the address of a symbol loaded in this process.
internal func sendSymbolAddress() {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  let name = readLine()!
  name.withCString {
    let handle = unsafeBitCast(Int(-2), to: UnsafeMutablePointer<Void>.self)
    let symbol = dlsym(handle, $0)
    let symbolAddress = unsafeBitCast(symbol, to: UInt.self)
    sendValue(symbolAddress)
  }
}

/// Send the length of a string to the parent.
internal func sendStringLength() {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  let address = readUInt()
  let cString = unsafeBitCast(address, to: UnsafePointer<CChar>.self)
  let count = String(validatingUTF8: cString)!.utf8.count
  sendValue(count)
}

/// Send the size of this architecture's pointer type.
internal func sendPointerSize() {
  debugLog("BEGIN \(#function)"); defer { debugLog("END \(#function)") }
  let pointerSize = UInt8(sizeof(UnsafePointer<Void>.self))
  sendValue(pointerSize)
}

/// Hold an `instance` and wait for the parent to query for information.
///
/// This is the main "run loop" of the test harness.
///
/// The parent will necessarily need to:
/// - Get the addresses of all of the reflection sections for any swift dylibs
///   that are loaded, where applicable.
/// - Get the address of the `instance`
/// - Get the pointer size of this process, which affects assumptions about the
///   the layout of runtime structures with pointer-sized fields.
/// - Read raw bytes out of this process's address space.
public func reflect(_ instance: AnyObject) {
  while let command = readLine(strippingNewline: true) {
    switch command {
    case String(validatingUTF8: RequestInstanceAddress)!:
      sendAddress(of: instance)
    case String(validatingUTF8: RequestReflectionInfos)!:
      sendReflectionInfos()
    case String(validatingUTF8: RequestReadBytes)!:
      sendBytes()
    case String(validatingUTF8: RequestSymbolAddress)!:
      sendSymbolAddress()
    case String(validatingUTF8: RequestStringLength)!:
      sendStringLength()
    case String(validatingUTF8: RequestPointerSize)!:
      sendPointerSize();
    case String(validatingUTF8: RequestExit)!:
      exit(EXIT_SUCCESS)
    default:
      fatalError("Unknown request received!")
    }
  }
}

/* Example usage

public protocol P {
  associatedtype Index
  var startIndex: Index { get }
}

public struct Thing : P {
  public let startIndex = 1010
}

public enum E<T: P> {
  case B(T)
  case C(T.Index)
}

public class A<T: P> : P {
  public let x: T?
  public let y: T.Index
  public let b = true
  public let t = (1, 1.0)
  private let type: NSObject.Type
  public let startIndex = 1010
  public init(x: T) {
    self.x = x
    self.y = x.startIndex
    self.type = NSObject.self
  }
}
let instance = A(x: A(x: Thing()))

reflect(A(x: Thing()))
*/
