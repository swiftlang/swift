// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-library-evolution -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -std=c++17)

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -std=c++17)

public enum AudioFileType {
    public enum WaveType {
        case THIS
        case THAT
    }

    public struct SubType {
        public var id: Int
    }

    case CAF(SubType), WAVE(WaveType)
}

public struct RecordConfig {
    public enum AudioFormat {
        case PCM, ALAC, AAC
    }

    public struct Directory {
        public var path: String?
    }

    public struct File {
        public var type: AudioFileType = .CAF(AudioFileType.SubType(id: 42))
        public var format: AudioFormat = .ALAC

        public struct Gate {
            public var prop: Double = -80.0
        }
    }

    public class Serializer {
        public init(_ x: Int) { self.id = x }
        public var id: Int
    }

    public var directory = Directory()
    public var file = File()
    public var gate = File.Gate()
}

public class AuxConfig {
    public struct AuxDirectory {
        public var path: String?
    }

    public var directory = AuxDirectory()
}

public func makeRecordConfig() -> RecordConfig {
    return RecordConfig()
}

public func makeAudioFileType() -> AudioFileType {
    return AudioFileType.CAF(AudioFileType.SubType(id: 42))
}

public class TestObject {
    enum CustomError: Swift.Error {
        case invalid
    }
}

extension RecordConfig.File {
    public func getFileExtension() -> String { ".wav" }
}

extension RecordConfig {
    public struct NestedInExtension {
        public var foo: Int
    }
}

extension AudioFileType {
    public struct NestedInExtension {
        public var foo: Int
    }
}


public func getFiles() -> [RecordConfig.File] {
    []
}
