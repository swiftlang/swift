// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-library-evolution -typecheck -module-name Classes -clang-header-expose-decls=all-public -emit-clang-header-path %t/classes.h
// RUN: %check-interop-cxx-header-in-clang(%t/classes.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -std=c++17)

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Classes -clang-header-expose-decls=all-public -emit-clang-header-path %t/classes.h
// RUN: %check-interop-cxx-header-in-clang(%t/classes.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -std=c++17)

public class RecordConfig {
    public enum AudioFormat {
        case PCM, ALAC, AAC
    }

    public struct Directory {
        public var path: String?
    }

    public class File {
        public var format: AudioFormat = .ALAC

        public class Gate {
            public init() {}
            public var prop: Int32 = 80

            public func computeValue() -> Int32 {
                return prop * 2
            }
        }
    }

    public var directory = Directory()
    public var file = File()
    public var gate = File.Gate()
}

public func makeRecordConfig() -> RecordConfig {
    return RecordConfig()
}
