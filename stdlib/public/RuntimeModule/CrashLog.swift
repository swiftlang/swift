//===--- CrashLog.swift ---------------------------------------*- swift -*-===//
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
//  This can ingest and egest crash logs that were sent as text or JSON.
//
//===----------------------------------------------------------------------===//

import Swift

@_spi(CrashLog)
@available(Backtracing 6.2, *)
public struct CrashLog<Address: FixedWidthInteger>: Codable {
    public struct Frame: Codable {
        enum CodingKeys: String, CodingKey {
            case kind
            case address
            case count
            case symbol
            case offset
            case description
            case image
            case sourceLocation
            case inlined
            case isSwiftRuntimeFailure = "runtimeFailure"
            case isSwiftThunk = "thunk"
            case isSystem = "system"
        }

        public enum Kind: String, Codable {
            case programCounter
            case returnAddress
            case asyncResumePoint
            case omittedFrames
            case truncated
        }

        public struct SourceLocation: Codable {
            public var file: String
            public var line: Int
            public var column: Int
        }

        // looking at swift-backtrace, SwiftBacktrace.outputJSONCrashLog.outputJSONThread
        // it looks like most of these fields are only populated if the backtrace is (at least partially) symbolicated.
        // the only one always populated in non-symbolicated backtraces is kind and usually address is too
        // (or count for omittedFrames)
        // even for (partially) symbolicated backtraces, some frames may not have a symbol
        public var kind: Kind
        public var address: String?
        /// count of ommitted frames, for kind == "omittedFrames"
        public var count: Int?
        public var symbol: String?
        public var offset: Int?
        public var description: String?
        public var image: String?
        public var sourceLocation: SourceLocation?
        // this variable is not intended to be directly streamed to/from JSON
        // but is used internally from capture to construct the description
        // for JSON
        public var demangledName: String?

        public var inlined: Bool = false
        public var isSwiftRuntimeFailure: Bool = false
        public var isSwiftThunk: Bool = false
        public var isSystem: Bool = false

        public var jsonBody: String {
            switch kind {
              case .programCounter:
                "\"kind\": \"programCounter\", \"address\": \"\(address ?? "")\""
              case .returnAddress:
                "\"kind\": \"returnAddress\", \"address\": \"\(address ?? "")\""
              case .asyncResumePoint:
                "\"kind\": \"asyncResumePoint\", \"address\": \"\(address ?? "")\""
              case .omittedFrames:
                "\"kind\": \"omittedFrames\", \"count\": \(count ?? 0)"
              case .truncated:
                "\"kind\": \"truncated\""
            }
        }

        public init(kind: Kind, address: String?) {
            self.kind = kind
            self.address = address
        }

        public init(from decoder: Decoder) throws {
            let values = try decoder.container(keyedBy: CodingKeys.self)
            kind = try values.decode(Kind.self, forKey: .kind)
            address = try values.decodeIfPresent(String.self, forKey: .address)
            count = try values.decodeIfPresent(Int.self, forKey: .count)
            symbol = try values.decodeIfPresent(String.self, forKey: .symbol)
            offset = try values.decodeIfPresent(Int.self, forKey: .offset)
            description = try values.decodeIfPresent(String.self, forKey: .description)
            image = try values.decodeIfPresent(String.self, forKey: .image)
            sourceLocation = try values.decodeIfPresent(SourceLocation.self, forKey: .sourceLocation)

            inlined = try values.decodeIfPresent(
                Bool.self,
                forKey: .inlined) ?? false

            isSwiftRuntimeFailure = try values.decodeIfPresent(
                Bool.self,
                forKey: .isSwiftRuntimeFailure) ?? false

            isSwiftThunk = try values.decodeIfPresent(
                Bool.self,
                forKey: .isSwiftThunk) ?? false

            isSystem = try values.decodeIfPresent(
                Bool.self,
                forKey: .isSystem) ?? false
        }
    }

    public struct Thread: Codable {
        enum CodingKeys: String, CodingKey {
            case name
            case crashed
            case registers
            case frames
        }

        public var name: String?
        public var crashed: Bool
        public var registers: [String:String]?
        public var frames: [Frame]

        @_spi(Formatting)
        public init(name: String?, crashed: Bool, registers: [String:String]?, frames: [Frame]) {
            self.name = name
            self.crashed = crashed
            self.registers = registers
            self.frames = frames
        }

        public init(from decoder: Decoder) throws {
            let values = try decoder.container(keyedBy: CodingKeys.self)
            name = try values.decodeIfPresent(String.self, forKey: .name)
            crashed = try values.decodeIfPresent(Bool.self, forKey: .crashed) ?? false
            registers = try values.decodeIfPresent([String:String].self, forKey: .registers)
            frames = try values.decode([Frame].self, forKey: .frames)
        }
    }

    public struct Image: Codable {
        // looking at swift-backtrace, SwiftBacktrace.outputJSONCrashLog.outputJSONCrashLog
        // the only mandatory fields for an image are baseAddress and endOfText
        public var name: String?
        public var buildId: String?
        public var path: String?
        public var baseAddress: String
        public var endOfText: String

        public init(
            name: String?,
            buildId: String?,
            path: String?,
            baseAddress: String,
            endOfText: String) {
                self.name = name
                self.buildId = buildId
                self.path = path
                self.baseAddress = baseAddress
                self.endOfText = endOfText
            }
    }

    // all of these fields are present in all crash logs from swift-backtrace
    // (at least, from all JSON crash logs)
    public var timestamp: String
    public var kind: String
    public var description: String
    public var faultAddress: String
    public var platform: String
    public var architecture: String
    public var threads: [Thread]

    public var capturedMemory: [String:String]?
    public var omittedImages: Int?
    public var images: [Image]?

    public var backtraceTime: Double

    public init(timestamp: String,
        kind: String,
        description: String,
        faultAddress: String,
        platform: String,
        architecture: String,
        threads: [Thread],
        capturedMemory: [String:String]?,
        omittedImages: Int?,
        images: [Image]?,
        backtraceTime: Double)
    {
        self.timestamp = timestamp
        self.kind = kind
        self.description = description
        self.faultAddress = faultAddress
        self.platform = platform
        self.architecture = architecture
        self.threads = threads
        self.capturedMemory = capturedMemory
        self.omittedImages = omittedImages
        self.images = images
        self.backtraceTime = backtraceTime
    }
}

@available(Backtracing 6.2, *)
extension Backtrace.Address {
    func hexRepresentation<Address:FixedWidthInteger>(nullAs: Address.Type) -> String {
        return hex(Address(self) ?? 0)
    }
}

@available(Backtracing 6.2, *)
extension CrashLog.Frame.SourceLocation {
    init?(_ symbol: SymbolicatedBacktrace.SourceLocation?) {
        guard let symbol else { return nil }

        file = symbol.path
        line = symbol.line
        column = symbol.column
    }

    func symbolicatedSourceLocation() -> SymbolicatedBacktrace.SourceLocation {
        .init(path: file, line: line, column: column)
    }
}

@available(Backtracing 6.2, *)
extension CrashLog.Frame {
    func richFrame() -> RichFrame<Address>? {
        switch (kind, CrashLog.addressFromString(address), count) {
            case (.programCounter, let addr?, _):
                .programCounter(addr)
            case (.returnAddress, let addr?, _):
                .returnAddress(addr)
            case (.asyncResumePoint, let addr?, _):
                .asyncResumePoint(addr)
            case (.omittedFrames, _, let ommittedFrames?):
                .omittedFrames(ommittedFrames)
            case (.truncated, _, _):
                .truncated
            default:
                nil
        }
    }

    func backtraceFrame() -> Backtrace.Frame? {
        switch (kind, address != nil ? Backtrace.Address(address!) : nil, count) {
        case (.programCounter, let address?, _):
            .programCounter(address)
        case (.returnAddress, let address?, _):
            .returnAddress(address)
        case (.asyncResumePoint, let address?, _):
            .asyncResumePoint(address)
        case (.omittedFrames, _, let count?):
            .omittedFrames(count)
        case (.truncated, _, _):
            .truncated
        default:
            nil
        }
    }

    func symbol(imageIndex: Int) -> SymbolicatedBacktrace.Symbol {
        .init(
            imageIndex: imageIndex,
            imageName: image ?? "???",
            rawName: symbol ?? "???",
            offset: offset ?? 0,
            sourceLocation: sourceLocation?.symbolicatedSourceLocation())
    }

    func symbolicatedFrame(imageIndex: Int) -> SymbolicatedBacktrace.Frame? {
        if let backtraceFrame = backtraceFrame() {
            .init(
                captured: backtraceFrame,
                symbol: symbol(imageIndex: imageIndex),
                inlined: inlined)
        } else {
            nil
        }
    }

    @_spi(Formatting)
    public init?(fromFrame frame: SymbolicatedBacktrace.Frame) {
        switch (frame.captured, frame.symbol) {
            case
            (.programCounter(let addr), let sbtSymbol?),
            (.returnAddress(let addr), let sbtSymbol?),
            (.asyncResumePoint(let addr), let sbtSymbol?):

            kind = switch frame.captured {
                case .programCounter: .programCounter
                case .returnAddress: .returnAddress
                case .asyncResumePoint: .asyncResumePoint
                default: fatalError("inconsistent state")
            }

            address = addr.hexRepresentation(nullAs: Address.self)
            symbol = sbtSymbol.rawName
            offset = sbtSymbol.offset
            description = sbtSymbol.description
            image = sbtSymbol.imageName
            sourceLocation = .init(sbtSymbol.sourceLocation)
            inlined = frame.inlined
            isSwiftRuntimeFailure = frame.isSwiftRuntimeFailure
            isSwiftThunk = frame.isSwiftThunk
            isSystem = frame.isSystem
            demangledName = sbtSymbol.name

            case (.omittedFrames(let count), _):
            self.count = count
            kind = .omittedFrames

            case (.truncated, _):
            kind = .truncated

            default: return nil
        }
    }

    @_spi(Formatting)
    public init?(fromFrame frame: Backtrace.Frame) {
        switch frame {
            case
            .programCounter(let addr),
            .returnAddress(let addr),
            .asyncResumePoint(let addr):

            kind = switch frame {
                case .programCounter: .programCounter
                case .returnAddress: .returnAddress
                case .asyncResumePoint: .asyncResumePoint
                default: fatalError("inconsistent state")
            }

            address = addr.hexRepresentation(nullAs: Address.self)

            case .omittedFrames(let count):
            self.count = count
            kind = .omittedFrames

            case .truncated:
            kind = .truncated
        }
    }
}

@available(Backtracing 6.2, *)
extension CrashLog.Image {
    func imageMapImage() -> ImageMap.Image {
        return .init(
            name: name,
            path: path,
            uniqueID: CrashLog.bytesFromHexString(buildId ?? ""),
            baseAddress: ImageMap.Address(CrashLog.addressFromString(baseAddress) ?? 0),
            endOfText: ImageMap.Address(CrashLog.addressFromString(endOfText) ?? 0)
            )
    }

    @_spi(Formatting)
    public init(fromImageMapImage image: ImageMap.Image) {
        self.name = image.name
        if let uniqueID = image.uniqueID {
            self.buildId = hex(uniqueID)
        }
        self.path = image.path
        self.baseAddress = hex(image.baseAddress)
        self.endOfText = hex(image.endOfText)
    }
}

@available(Backtracing 6.2, *)
public extension CrashLog.Thread {
    func backtrace(architecture: String, images: ImageMap?) -> Backtrace {
        let frames = self.frames.compactMap { $0.richFrame() }
        return Backtrace(architecture: architecture, frames: frames, images: images)
    }

    func symbolicatedBacktrace(
        architecture: String,
        images: ImageMap?,
        platform: Backtrace.SymbolicationPlatform) 
    -> SymbolicatedBacktrace? {
        guard let images else { return nil }
        let backtrace = backtrace(architecture: architecture, images: images)
        let frames = self.frames.compactMap { frame in
            frame.symbolicatedFrame(imageIndex:
                images.images.firstIndex(where:
                    { $0.name == frame.image }
                ) ?? -1
            )
        }
        return .init(backtrace: backtrace, images: images, frames: frames)
    }

    mutating func updateWithBacktrace(symbolicatedBacktrace: SymbolicatedBacktrace) {
        frames = symbolicatedBacktrace.frames.compactMap { CrashLog.Frame(fromFrame: $0) }
    }
}

@available(Backtracing 6.2, *)
extension Context {
  static var addressType: any FixedWidthInteger.Type { Address.self }
}

@available(Backtracing 6.2, *)
extension CrashLog {
    private static func context(forArchitecture arch: String) -> (any Context.Type)? {
        switch arch {
            case "arm": return ARMContext.self
            case "arm64": return ARM64Context.self
            case "i386": return I386Context.self
            case "x86_64": return X86_64Context.self
            default: return nil
        }
    }

    private static func wordSize(forAddressType addr: any FixedWidthInteger.Type) -> ImageMap.WordSize? {
        switch addr.bitWidth {
            case 16: return .sixteenBit
            case 32: return .thirtyTwoBit
            case 64: return .sixtyFourBit
            default: return nil
        }
    }

    public mutating func symbolicate(
        allThreads: Bool = false,
        platform: Backtrace.SymbolicationPlatform,
        alternativeSymbolFilePaths: [String],
        options: Backtrace.SymbolicationOptions = .default) {

        let images = imageMap()

        func symbolicateThread(_ thread: CrashLog.Thread) -> CrashLog.Thread {
            var thread = thread
            let backtrace: Backtrace = thread.backtrace(architecture: architecture, images: images)

            if let symbolicatedBacktrace = backtrace.symbolicated(
                with: images,
                platform: platform,
                alternativeSymbolFilePaths: alternativeSymbolFilePaths,
                options: options) {

                thread.updateWithBacktrace(symbolicatedBacktrace: symbolicatedBacktrace)
            }
            return thread
        }

        let symbolicatedThreads = threads.map {
            if allThreads || $0.crashed {
                symbolicateThread($0)
            } else {
                $0
            }
        }

        self.threads = symbolicatedThreads
    }

    @_spi(Testing)
    @_spi(Formatting)
    public static func wordSize(forArchitecture arch: String) -> ImageMap.WordSize? {
        guard let context = context(forArchitecture: arch) else { return nil }
        return wordSize(forAddressType: context.addressType)
    }

    @_spi(Testing)
    @_spi(Formatting)
    public static func addressFromString(_ address: String?) -> Address? {
        func trimOx(_ hex: String) -> String {
            if hex.hasPrefix("0x") { String(hex.dropFirst(2)) } else { hex }
        }

        guard let address,
         let addressValue = Address(trimOx(address), radix: 16) else {
            return nil
        }
        
        return addressValue
    }

    @_spi(Testing)
    @_spi(Formatting)
    public static func bytesFromHexString(_ hexString: any StringProtocol) -> [UInt8] {
        guard hexString.count >= 2 else {
            return []
        }

        let i = hexString.startIndex
        let j = hexString.index(after: i)
        let byteString = hexString[i...j]

        if let byte = UInt8(byteString, radix: 16) {
            return [byte] + bytesFromHexString(hexString.dropFirst(2))
        } else {
            return []
        }
    }

    public func imageMap() -> ImageMap? {
        guard let images,
        let wordSize = Self.wordSize(forArchitecture: architecture) else { return nil }

        var imageMapImages = images.map { $0.imageMapImage() }
        // sort the images in case they somehow ended up out of order in the crash log
        imageMapImages.sort(by: { $0.baseAddress < $1.baseAddress })

        return ImageMap(platform: platform, images: imageMapImages, wordSize: wordSize)
    }
}
