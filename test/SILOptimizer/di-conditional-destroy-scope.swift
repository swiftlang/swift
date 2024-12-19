// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s -Onone -Xllvm \
// RUN:   -sil-print-after=raw-sil-inst-lowering -Xllvm \
// RUN:   -sil-print-functions=$s2fs36RecursibleDirectoryContentsGeneratorC4path10fileSystemAcA12AbsolutePathV_AA04FileH0_ptKc33_F8B132991B28208F48606E87DC165393Llfc \
// RUN:   -Xllvm -sil-print-types -Xllvm -sil-print-debuginfo -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: objc_interop


// CHECK: [[ADR:%.*]] = ref_element_addr %{{.*}} : $RecursibleDirectoryContentsGenerator, #RecursibleDirectoryContentsGenerator.fileSystem, loc {{.*}}:39:5, scope [[SCOPE:[0-9]+]]
// CHECK: [[ADR_ACCESS:%.*]] = begin_access [deinit] [static] [[ADR]]
// CHECK: destroy_addr [[ADR_ACCESS]] : $*any FileSystem, loc {{.*}}:39:5, scope [[SCOPE]]


import Foundation

public struct AbsolutePath {
    public init(_ absStr: String) {}
}

public protocol FileSystem: class {
    func getDirectoryContents(_ path: AbsolutePath) throws -> [String]
}
public extension FileSystem {
}

public var currentWorkingDirectory: AbsolutePath {
    let cwdStr = FileManager.default.currentDirectoryPath
    return AbsolutePath(cwdStr)
}
public class RecursibleDirectoryContentsGenerator {
    private var current: (path: AbsolutePath, iterator: IndexingIterator<[String]>)
    private let fileSystem: FileSystem
    fileprivate init(
        path: AbsolutePath,
        fileSystem: FileSystem
    ) throws {
        self.fileSystem = fileSystem
        current = (path, try fileSystem.getDirectoryContents(path).makeIterator())
    }
}
