// RUN: %target-swift-frontend -emit-sil %s -Onone -Xllvm \
// RUN:   -sil-print-after=definite-init -Xllvm \
// RUN:   -sil-print-only-functions=$S2fs36RecursibleDirectoryContentsGeneratorC4path10fileSystemAcA12AbsolutePathV_AA04FileH0_ptKc33_F8B132991B28208F48606E87DC165393Llfc \
// RUN:   -Xllvm -sil-print-debuginfo -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: objc_interop


// CHECK: %49 = ref_element_addr %48 : $RecursibleDirectoryContentsGenerator, #RecursibleDirectoryContentsGenerator.fileSystem, loc {{.*}}:38:5, scope 2
// CHECK:  destroy_addr %49 : $*FileSystem, loc {{.*}}:38:5, scope 2


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
