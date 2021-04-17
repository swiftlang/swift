# Swift implemented in Swift

_Libswift_ is the part of the Swift compiler, which is implemented in Swift.

With _libswift_ it is possible to add SIL optimization passes written in Swift. It allows to gradually migrate the SIL optimizer from C++ to Swift.

## Building

_Libswift_ is a static library and it is built as part of the swift compiler using build-script and CMake.

To enable _libswift_, add the build-script option `--libswift`.

In this early stage of development _libswift_ is disabled by default. Right now _libswift_ does not contain any optimizations or features which are not available in the existing optimizer. Therefore a compiler with disabled _libswift_ still behaves as a compiler with enabled _libswift_. This will change soon.

When _libswift_ is enabled, it is built with a Swift toolchain (5.3 or newer), which must be installed on the host system. The `swiftc` compiler driver is expected to be in the command search path.

Currently the `swift-frontend` and `sil-opt` tools use _libswift_. Tools, which don't use any optimization passes from _libswift_ don't need to link _libswift_. For example, all tools, which compile Swift source code, but don't optimize it, like SourceKit or lldb, don't need to link _libswift_. As long as `initializeLibSwift()` is not called there is no dependency on _libswift_.

This also means that currently it is not possible to implement mandatory passes in _libswift_, because this would break tools which compile Swift code but don't use _libswift_. When we want to implement mandatory passes in _libswift_ in the future, we'll need to link _libswift_ to all those tools.


