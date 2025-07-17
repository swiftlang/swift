//===--- ImageMap+Win32.swift ---------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Windows specifics for ImageMap capture.
//
//===----------------------------------------------------------------------===//

#if os(Windows)

import Swift

internal import WinSDK
internal import BacktracingImpl.ImageFormats.CodeView

typealias CV_PDB70_INFO = swift.runtime.CV_PDB70_INFO

let hNtDll = GetModuleHandleA("ntdll.dll")

func GetNtFunc<T>(_ name: String) -> T {
  guard let result = GetProcAddress(hNtDll, name) else {
    fatalError("Unable to look up \(name) in ntdll")
  }
  return unsafeBitCast(result, to: T.self)
}

let pfnRtlGetVersion: @convention(c) (UnsafeMutablePointer<RTL_OSVERSIONINFOW>) -> NTSTATUS = GetNtFunc("RtlGetVersion")

fileprivate func RtlGetVersion(
  _ versionInfo: inout RTL_OSVERSIONINFOW
) -> NTSTATUS {
  versionInfo.dwOSVersionInfoSize = DWORD(MemoryLayout<RTL_OSVERSIONINFOW>.size)

  return pfnRtlGetVersion(&versionInfo)
}

fileprivate func GetModuleBaseName(
  _ hProcess: HANDLE,
  _ hModule: HMODULE
) -> String? {
  return withUnsafeTemporaryAllocation(of: WCHAR.self, capacity: 1024) {
    buffer in

    let dwRet = K32GetModuleBaseNameW(hProcess, hModule,
                                      buffer.baseAddress,
                                      DWORD(buffer.count))
    if dwRet == 0 {
      return nil
    }

    let slice = buffer[0..<Int(dwRet)]

    return String(decoding: slice, as: UTF16.self)
  }
}

fileprivate func GetModuleFileNameEx(
  _ hProcess: HANDLE,
  _ hModule: HMODULE
) -> String? {
  return withUnsafeTemporaryAllocation(of: WCHAR.self, capacity: 32768) {
    buffer in

    let dwRet = K32GetModuleFileNameExW(hProcess, hModule,
                                        buffer.baseAddress,
                                        DWORD(buffer.count))
    if dwRet == 0 {
      return nil
    }

    let slice = buffer[0..<Int(dwRet)]

    return String(decoding: slice, as: UTF16.self)
  }
}

let EnumProcessModules = K32EnumProcessModules
let GetModuleInformation = K32GetModuleInformation

extension ImageMap.Image: Equatable {
  static func == (lhs: Self, rhs: Self) -> Bool {
    return (lhs.name == rhs.name
      && lhs.path == rhs.path
      && lhs.uniqueID == rhs.uniqueID
      && lhs.baseAddress == rhs.baseAddress
      && lhs.endOfText == rhs.endOfText
    )
  }

  static func != (lhs: Self, rhs: Self) -> Bool {
    return !(lhs == rhs)
  }
}

extension ImageMap.Image {
  func hash(into hasher: inout Hasher) {
    hasher.combine(name)
    hasher.combine(path)
    hasher.combine(uniqueID)
    hasher.combine(baseAddress)
    hasher.combine(endOfText)
  }
}

// This is OK to be Sendable, because it's immutable once we've located
// the data.  We're only using a class so that we can deallocate it.
class ExceptionTableWrapper<T>: @unchecked Sendable {
  var table: UnsafeBufferPointer<T>
  var shouldDeallocate: Bool

  init(table: UnsafeBufferPointer<T>, allocated: Bool = true) {
    self.table = table
    self.shouldDeallocate = allocated
  }

  deinit {
    if shouldDeallocate {
      table.deallocate()
    }
  }
}

extension ImageMap {

  func exceptionTable(at imageNdx: Int) -> ExceptionTable? {
    return images[imageNdx].exceptionTable
  }

  private static let platform = {
    var versionInfo = RTL_OSVERSIONINFOW()
    if RtlGetVersion(&versionInfo) == 0 {
      // Why they decided to report Windows 11 as version 10.0 I'm not sure.
      if versionInfo.dwMajorVersion == 10 && versionInfo.dwBuildNumber >= 21996 {
        versionInfo.dwMajorVersion = 11
      }
      return "Windows \(versionInfo.dwMajorVersion).\(versionInfo.dwMinorVersion) build \(versionInfo.dwBuildNumber)"
    } else {
      return "Windows (unknown)"
    }
  }()

  private static func fetchExceptionTable<T>(
    hProcess: HANDLE,
    from address: Address,
    size: UInt64,
    as: T.Type
  ) -> ExceptionTableWrapper<T>? {
    let entrySize = MemoryLayout<T>.stride
    let count = Int(size) / entrySize

    // If we're looking at the current process, return a reference to
    // the data in memory, rather than copying
    if hProcess == GetCurrentProcess() {
      let base = UnsafePointer<T>(bitPattern: UInt(address))
      let table = UnsafeBufferPointer<T>(start: base, count: count)
      return ExceptionTableWrapper(table: table, allocated: false)
    }

    // Otherwise, we need to fetch from the remote process, so allocate space
    let buffer = UnsafeMutableBufferPointer<T>.allocate(capacity: count)

    var cbRead = SIZE_T(0)
    if ReadProcessMemory(hProcess,
                         UnsafeRawPointer(bitPattern: UInt(address)),
                         UnsafeMutableRawPointer(buffer.baseAddress),
                         SIZE_T(buffer.count * entrySize),
                         &cbRead) {
      let initializedCount = Int(cbRead) / entrySize
      let table = UnsafeBufferPointer(rebasing: buffer[0..<initializedCount])
      return ExceptionTableWrapper(table: table, allocated: true)
    }

    return nil
  }

  @_spi(Internal)
  public static func capture(for process: UInt) -> ImageMap {
    let hProcess = HANDLE(bitPattern: process)!

    // Get a list of the modules loaded into the specified process
    var hModules = Array<HMODULE?>(repeating: nil, count: 64)

    while true {
      var cbNeeded = DWORD(MemoryLayout<HMODULE>.stride * hModules.count)
      let cb = cbNeeded

      let result = hModules.withUnsafeMutableBufferPointer{ buffer in
        EnumProcessModules(hProcess,
                           buffer.baseAddress,
                           cb,
                           &cbNeeded)
      }
      if !result {
        break
      }
      if cb >= cbNeeded {
        let count = Int(cbNeeded) / MemoryLayout<HMODULE>.stride
        if hModules.count > count {
          hModules.removeSubrange(count..<hModules.count)
        }
        break
      }

      let needed = Int(cbNeeded) / MemoryLayout<HMODULE>.stride
      hModules.reserveCapacity(needed)
      hModules.append(contentsOf:
                        repeatElement(nil, count: needed - hModules.count))
    }

    #if DEBUG_WIN32_IMAGEMAP_CAPTURE
    print("Got \(hModules.count) modules")
    #endif

    // Turn that into a list of Images
    var images: [Image] = []
    for hModule in hModules {
      let moduleName = GetModuleBaseName(hProcess, hModule!)
      let modulePath = GetModuleFileNameEx(hProcess, hModule!)
      var moduleInfo = MODULEINFO()

      guard GetModuleInformation(hProcess, hModule!, &moduleInfo,
                                 DWORD(MemoryLayout<MODULEINFO>.size)) else {
        #if DEBUG_WIN32_IMAGEMAP_CAPTURE
        print("GetModuleInformation() failed for \(hModule!)")
        #endif
        continue
      }

      var theUUID: [UInt8]? = nil
      let baseAddress = Address(UInt(bitPattern: moduleInfo.lpBaseOfDll))
      let endOfText: Address
      var debugEntry = IMAGE_DATA_DIRECTORY()
      var exceptionEntry = IMAGE_DATA_DIRECTORY()

      var wMagic: WORD = 0
      var dwOffset: DWORD = 0
      var cbRead: SIZE_T = 0

      // Read and check the PE signature
      var bRet = withUnsafeMutablePointer(to: &wMagic) { ptr in
        ReadProcessMemory(hProcess, moduleInfo.lpBaseOfDll,
                          ptr, SIZE_T(MemoryLayout<WORD>.size),
                          &cbRead)
      }
      if !bRet || wMagic != 0x5a4d {
        #if DEBUG_WIN32_IMAGEMAP_CAPTURE
        if !bRet {
          print("Unable to read PE signature for \(hModule!)")
        } else {
          print("Bad PE magic \(hex(wMagic)) for \(hModule!)")
        }
        #endif
        continue
      }

      // Get the offset to the image headers
      bRet = withUnsafeMutablePointer(to: &dwOffset) { ptr in
        ReadProcessMemory(hProcess, moduleInfo.lpBaseOfDll + 0x3c,
                          ptr, SIZE_T(MemoryLayout<DWORD>.size),
                          &cbRead)
      }
      if !bRet {
        #if DEBUG_WIN32_IMAGEMAP_CAPTURE
        print("Unable to read image header offset for \(hModule!)")
        #endif
        continue
      }

      // Read the COFF magic number
      var dwMagic2: DWORD = 0
      bRet = withUnsafeMutablePointer(to: &dwMagic2) { ptr in
        ReadProcessMemory(hProcess, moduleInfo.lpBaseOfDll + Int(dwOffset),
                          ptr, SIZE_T(MemoryLayout<DWORD>.size),
                          &cbRead)
      }
      if !bRet || dwMagic2 != 0x4550 {
        #if DEBUG_WIN32_IMAGEMAP_CAPTURE
        if !bRet {
          print("Unable to read PE signature for \(hModule!)")
        } else {
          print("Bad COFF magic \(hex(dwMagic2)) for \(hModule!)")
        }
        #endif
        continue
      }

      // Now read the IMAGE_FILE_HEADER
      var header = IMAGE_FILE_HEADER()
      bRet = withUnsafeMutablePointer(to: &header) { ptr in
        ReadProcessMemory(hProcess, moduleInfo.lpBaseOfDll + Int(dwOffset + 4),
                          ptr, SIZE_T(MemoryLayout<IMAGE_FILE_HEADER>.size),
                          &cbRead)
      }
      if !bRet {
        #if DEBUG_WIN32_IMAGEMAP_CAPTURE
        print("Unable to read image file header for \(hModule!)")
        #endif
        continue
      }

      if (header.Characteristics & WORD(IMAGE_FILE_32BIT_MACHINE)) != 0 {
        // 32-bit
        var optionalHeader = IMAGE_OPTIONAL_HEADER32()

        #if DEBUG_WIN32_IMAGEMAP_CAPTURE
        print("\(hModule!) is 32-bit")
        #endif

        bRet = withUnsafeMutablePointer(to: &optionalHeader) { ptr in
          ReadProcessMemory(hProcess, moduleInfo.lpBaseOfDll + Int(dwOffset + 4)
                                        + MemoryLayout<IMAGE_FILE_HEADER>.size,
                            ptr,
                            SIZE_T(MemoryLayout<IMAGE_OPTIONAL_HEADER32>.size),
                            &cbRead)
        }
        if !bRet {
          #if DEBUG_WIN32_IMAGEMAP_CAPTURE
          print("Unable to read optional header for \(hModule!)")
          #endif
          continue
        }

        endOfText = baseAddress + Address(optionalHeader.SizeOfCode)

        // 3 is IMAGE_DIRECTORY_ENTRY_EXCEPTION
        exceptionEntry = optionalHeader.DataDirectory.3

        // 6 is IMAGE_DIRECTORY_ENTRY_DEBUG
        debugEntry = optionalHeader.DataDirectory.6
      } else {
        // 64-bit
        var optionalHeader = IMAGE_OPTIONAL_HEADER64()

        #if DEBUG_WIN32_IMAGEMAP_CAPTURE
        print("\(hModule!) is 64-bit")
        #endif

        bRet = withUnsafeMutablePointer(to: &optionalHeader) { ptr in
          ReadProcessMemory(hProcess, moduleInfo.lpBaseOfDll + Int(dwOffset + 4)
                                        + MemoryLayout<IMAGE_FILE_HEADER>.size,
                            ptr,
                            SIZE_T(MemoryLayout<IMAGE_OPTIONAL_HEADER64>.size),
                            &cbRead)
        }
        if !bRet {
          #if DEBUG_WIN32_IMAGEMAP_CAPTURE
          print("Unable to read optional header for \(hModule!)")
          #endif
          continue
        }

        endOfText = baseAddress + Address(optionalHeader.SizeOfCode)

        // 3 is IMAGE_DIRECTORY_ENTRY_EXCEPTION
        exceptionEntry = optionalHeader.DataDirectory.3

        // 6 is IMAGE_DIRECTORY_ENTRY_DEBUG
        debugEntry = optionalHeader.DataDirectory.6
      }

      var fpoEntry: IMAGE_DEBUG_DIRECTORY? = nil

      // If there's a debug directory, scan for the UUID
      if debugEntry.Size > 0 {
        withUnsafeTemporaryAllocation(
          of: IMAGE_DEBUG_DIRECTORY.self,
          capacity: Int(debugEntry.Size)
            / MemoryLayout<IMAGE_DEBUG_DIRECTORY>.stride
        ) { buffer in
          let address = moduleInfo.lpBaseOfDll + Int(debugEntry.VirtualAddress)
          if ReadProcessMemory(
               hProcess,
               address,
               buffer.baseAddress,
               SIZE_T(buffer.count * MemoryLayout<IMAGE_DEBUG_DIRECTORY>.stride),
               &cbRead
             ) {
            // We read the debug directory data successfully; scan it.
            //
            // We are looking for either IMAGE_DEBUG_TYPE_REPRO with a payload,
            // or IMAGE_DEBUG_TYPE_CODEVIEW with the signature RSDS, which
            // contains a GUID.
            //d
            // We also keep track of FPO data for 32-bit x86.
            //
            // Prefer IMAGE_DEBUG_TYPE_REPRO as that's the best kind of hash.
            var gotUUID = false
            for entry in buffer {
              if entry.SizeOfData == 0 {
                continue
              }

              // IMAGE_DEBUG_TYPE_REPRO with a payload
              if entry.Type == IMAGE_DEBUG_TYPE_REPRO && !gotUUID {
                var dwHashLen = DWORD(0)
                bRet = withUnsafeMutablePointer(to: &dwHashLen) { ptr in
                  ReadProcessMemory(hProcess,
                                    moduleInfo.lpBaseOfDll + Int(entry.AddressOfRawData),
                                    ptr, SIZE_T(MemoryLayout<DWORD>.size),
                                    &cbRead)
                }

                if bRet {
                  var uuid = Array<UInt8>(repeating: 0, count: Int(dwHashLen))

                  bRet = uuid.withUnsafeMutableBufferPointer { buffer in
                    ReadProcessMemory(hProcess,
                                      moduleInfo.lpBaseOfDll + Int(entry.AddressOfRawData) + 4,
                                      buffer.baseAddress, SIZE_T(buffer.count),
                                      &cbRead)
                  }

                  if bRet {
                    theUUID = uuid

                    // Do not set gotUUID here; we want IMAGE_DEBUG_TYPE_CODEVIEW
                    // to take precedence.
                  }
                }
              }

              // IMAGE_DEBUG_TYPE_CODEVIEW
              if entry.Type == IMAGE_DEBUG_TYPE_CODEVIEW && !gotUUID {
                var pdbInfo = CV_PDB70_INFO()
                bRet = withUnsafeMutablePointer(to: &pdbInfo) { ptr in
                  ReadProcessMemory(hProcess,
                                    moduleInfo.lpBaseOfDll + Int(entry.AddressOfRawData),
                                    ptr, SIZE_T(MemoryLayout<CV_PDB70_INFO>.size),
                                    &cbRead)
                }

                if bRet {
                  withUnsafeBytes(of: pdbInfo.Signature) {
                    theUUID = Array($0)
                  }
                  withUnsafeBytes(of: pdbInfo.dwAge) {
                    theUUID!.append(contentsOf: $0)
                  }

                  gotUUID = true
                }
              }

              // IMAGE_DEBUG_TYPE_FPO
              if entry.Type == IMAGE_DEBUG_TYPE_FPO {
                fpoEntry = entry
              }
            }
          }
        }
      }

      let exceptionTable: ExceptionTable?
      switch header.Machine {
      case WORD(IMAGE_FILE_MACHINE_ARM64):
        let tableAddress = (UInt(bitPattern: moduleInfo.lpBaseOfDll)
                              + UInt(exceptionEntry.VirtualAddress))
        if let table = fetchExceptionTable(hProcess: hProcess,
                              from: Address(tableAddress),
                              size: UInt64(exceptionEntry.Size),
                              as: IMAGE_ARM64_RUNTIME_FUNCTION_ENTRY.self) {
          exceptionTable = .arm64(table)
        } else {
          exceptionTable = nil
        }
      case WORD(IMAGE_FILE_MACHINE_AMD64):
        let tableAddress = (UInt(bitPattern: moduleInfo.lpBaseOfDll)
                              + UInt(exceptionEntry.VirtualAddress))
        if let table = fetchExceptionTable(hProcess: hProcess,
                              from: Address(tableAddress),
                              size: UInt64(exceptionEntry.Size),
                              as: _IMAGE_RUNTIME_FUNCTION_ENTRY.self) {
          exceptionTable = .amd64(table)
        } else {
          exceptionTable = nil
        }
      case WORD(IMAGE_FILE_MACHINE_I386):
        if let fpoEntry {
          let tableAddress = (UInt(bitPattern: moduleInfo.lpBaseOfDll)
                                + UInt(fpoEntry.AddressOfRawData))
          if let table = fetchExceptionTable(hProcess: hProcess,
                                from: Address(tableAddress),
                                size: UInt64(fpoEntry.SizeOfData),
                                as: FPO_DATA.self) {
            exceptionTable = .i386(table)
          } else {
            exceptionTable = nil
          }
        } else {
          exceptionTable = nil
        }
      default:
        exceptionTable = nil
      }

      #if DEBUG_WIN32_IMAGEMAP_CAPTURE
      print("Added image \(moduleName) for \(hModule!)")
      #endif

      images.append(Image(name: moduleName,
                          path: modulePath,
                          uniqueID: theUUID,
                          baseAddress: baseAddress,
                          endOfText: endOfText,
                          exceptionTable: exceptionTable))
    }

    images.sort(by: { $0.baseAddress < $1.baseAddress })

    let wordSize: WordSize
    var machine = USHORT(0)
    var nativeMachine = USHORT(0)

    if IsWow64Process2(hProcess, &machine, &nativeMachine) {
      switch CInt(machine) {
      case IMAGE_FILE_MACHINE_I386,
           IMAGE_FILE_MACHINE_ARM:
        wordSize = .thirtyTwoBit
      case IMAGE_FILE_MACHINE_AMD64,
           IMAGE_FILE_MACHINE_ARM64:
        wordSize = .sixtyFourBit
      default:
        // Guess that any new machine types will be 64-bit
        wordSize = .sixtyFourBit
      }
    } else {
      #if arch(x86_64) || arch(arm64)
      wordSize = .sixtyFourBit
      #elseif arch(i386) || arch(arm)
      wordSize = .thirtyTwoBit
      #endif
    }

    return ImageMap(
      platform: ImageMap.platform,
      images: images,
      wordSize: wordSize
    )
  }

}

#endif // os(Windows)
