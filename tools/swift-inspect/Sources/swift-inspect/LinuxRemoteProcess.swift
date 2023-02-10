#if os(Linux) 

import SwiftRemoteMirror
import MemtoolCore
import Cutils // Part of Memtool

internal final class Linux64RemoteProcess: RemoteProcess {
    typealias ProcessIdentifier = Cutils.pid_t
    typealias ProcessHandle = Cutils.pid_t

    var process: ProcessHandle
    var context: SwiftReflectionContextRef!
    let memtoolSession: MemtoolCore.Session
    let memtoolHeapAnalysis: MemtoolCore.GlibcMallocAnalyzer

    static var QueryDataLayout: QueryDataLayoutFunction {
        return { (context, type, _, output) in
            #if DIAGNOSTIC
            print("Request [Query Layout]: \(type)")
            #endif

            switch type {
            case DLQ_GetPointerSize:
                let size = UInt8(MemoryLayout<UnsafeRawPointer>.stride)
                output?.storeBytes(of: size, toByteOffset: 0, as: UInt8.self)
                return 1

            case DLQ_GetSizeSize:
                // FIXME(compnerd) support 32-bit processes
                let size = UInt8(MemoryLayout<UInt64>.stride)
                output?.storeBytes(of: size, toByteOffset: 0, as: UInt8.self)
                return 1

            case DLQ_GetLeastValidPointerValue:
                let value: UInt64 = 0x1000
                output?.storeBytes(of: value, toByteOffset: 0, as: UInt64.self)
                return 1

            default:
                return 0
            }
        }
    }

    static var Free: FreeFunction {
        return { (_, bytes, _) in
            #if DIAGNOSTIC
            print("Request [free]: \(String(describing: bytes))")
            #endif
            free(UnsafeMutableRawPointer(mutating: bytes))
        }
    }

    static var ReadBytes: ReadBytesFunction {
        return { (context, baseAddress, size, _) in 
            let process = Linux64RemoteProcess.fromOpaque(context!)
            let bytes = UnsafeRawPointer(Cutils.swift_inspect_bridge__ptrace_peekdata(process.process, baseAddress, size))
            #if DIAGNOSTIC
            print("Request [Load bytes]: \(baseAddress.hexa) size \(size)")
            #endif
            return bytes
        }
    }

    static var GetStringLength: GetStringLengthFunction {
        return { (context, baseAddress) in
            let process = Linux64RemoteProcess.fromOpaque(context!)
            let uintBase = UInt(baseAddress)
            
            #if DIAGNOSTIC
            print("Request [Sting length]: \(baseAddress.hexa) ", terminator: "")
            #endif

            guard let candidateRegion = process.memtoolSession.map?.first(where: { $0.range.contains(uintBase) }) else {
                #if DIAGNOSTIC
                print("returned 0; outside any region")
                #endif

                return 0
            }

            // Requirement to avoid reading from read-only space was dropped due to frequent attempts to read wq space
            guard 
                candidateRegion.properties.flags.contains(.read) 
            else {
                #if DIAGNOSTIC
                print("returned 0; region \(candidateRegion) is not read-only")
                #endif

                return 0

            }

            var currentAddress = baseAddress
            var length: UInt64 = 0

            #if DIAGNOSTIC
            var bytes: [CChar] = []
            #endif

            while currentAddress < candidateRegion.range.upperBound {
                let result = Cutils.swift_inspect_bridge__ptrace_peekdata(process.process, currentAddress, 1)
                guard let char = result?.bindMemory(to: CChar.self, capacity: 1).pointee else {
                    #if DIAGNOSTIC
                    print("returned 0; reading failed")
                    #endif
                    return 0
                }
                result?.deallocate()
                if char == 0 {
                    #if DIAGNOSTIC
                    print("returned \(length); \(bytes)")
                    #endif
                    return length
                }
                
                #if DIAGNOSTIC
                bytes.append(char)
                #endif

                length += 1
                currentAddress += 1
            }

            #if DIAGNOSTIC
            print("reached end of allowed memory; returned \(length); \(bytes)")
            #endif
            
            return length
        }
    }

    static var GetSymbolAddress: GetSymbolAddressFunction { 
        return { (context, symbol, length) in 
            let process = Linux64RemoteProcess.fromOpaque(context!)

            #if DIAGNOSTIC
            print("Request [Get address]: ", terminator: "")
            #endif

            guard let symbol = symbol else { print("null"); return 0 }
            let name: String = symbol.withMemoryRebound(to: UInt8.self, capacity: Int(length)) {
                let buffer = UnsafeBufferPointer(start: $0, count: Int(length))
                return String(decoding: buffer, as: UTF8.self)
            }
            for symbol in process.memtoolSession.symbols ?? [] where symbol.properties.name == name {
                #if DIAGNOSTIC
                print("name \(name); symbol {\(symbol.hexa)}")
                #endif
                return UInt64(symbol.range.lowerBound)
            }

            #if DIAGNOSTIC
            print("name \(name); failed to load")
            #endif

            return 0
        }
    }

    func symbolicate(_ address: swift_addr_t) -> (module: String?, symbol: String?) {
        for symbol in memtoolSession.symbols ?? [] where symbol.range.contains(0){
            #if DIAGNOSTIC
            print("Request [Get symbol]: \(address.hexa) {\(symbol.hexa)}")
            #endif
            return (symbol.properties.segment.rawValue, symbol.properties.name)
        }
        #if DIAGNOSTIC
        print("Request [Get symbol]: \(address.hexa) failed")
        #endif
        return (nil, nil)
    }

    func iterateHeap(_ body: (swift_addr_t, UInt64) -> Void) {
        #if DIAGNOSTIC
        print("Request [Iterate heap]:")
        #endif
        for heapItem in memtoolHeapAnalysis.exploredHeap {
            guard case let .mallocChunk(state) = heapItem.properties.rebound, state.isActive else {
                continue
            }
            do {
                let chunk = try memtoolSession.checkedChunk(baseAddress: heapItem.range.lowerBound)
                #if DIAGNOSTIC
                print("Will iterate \(chunk.header); \(chunk.chunkAllocatedRange.lowerBound.hexa); buffer: \(chunk.content.asAsciiString)")
                #endif
                body(UInt64(chunk.content.segment.lowerBound), UInt64(chunk.content.segment.unsignedCount))
            } catch {
                // FIXME(stuchlej) log error
            }
        }

    }

    init?(processId: ProcessHandle) {
        self.process = processId
        // FIXME(stuchlej) Should I place `wait` here?

        let processSession = MemtoolCore.ProcessSession(pid: processId)
        self.memtoolSession = processSession
        processSession.loadMap()
        processSession.loadSymbols()
        processSession.loadThreads()

        // Some symbols are prefixed with `.protected ` string, that is not dealt with by memtool
        for i in 0..<(processSession.symbols?.count ?? 0) {
            processSession.symbols?[i].properties.name.trimPrefix(".protected ")
        }

        do {
            self.memtoolHeapAnalysis = try GlibcMallocAnalyzer(session: processSession)
            try memtoolHeapAnalysis.analyze()
        } catch {
            // FIXME(stuchlej) log error
            return nil
        }

        guard 
            let context = swift_reflection_createReflectionContextWithDataLayout(
                self.toOpaqueRef(), 
                Self.QueryDataLayout, 
                Self.Free, 
                Self.ReadBytes, 
                Self.GetStringLength, 
                Self.GetSymbolAddress
            )
        else {
            // FIXME(stuchlej) log error
            return nil
        }
        self.context = context
    }

    deinit {
        swift_reflection_destroyReflectionContext(context)
    }
}


#if DIAGNOSTIC
import Foundation

extension UInt {
    var hexa: String {
        String(format: "0x%016lx", self)
    }
}

extension UInt64 {
    var hexa: String {
        String(format: "0x%016lx", self)
    }
}

extension MemoryRegion {
    private struct StringRegion<T> {
        let range: String
        let properties: T
    }

    var hexa: String {
        "\(StringRegion(range: range.lowerBound.hexa + "..<" + range.upperBound.hexa, properties: properties))"
    }
}
#endif

#endif /* os(Linux) */
