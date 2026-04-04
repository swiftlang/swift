# Swift SDK Build

Building a `Swift SDK` involves cross-compiling [Swift Core libraries](https://www.swift.org/documentation/core-libraries/) and `Swift standard library`. It allows building and maintaining custom Swift SDKs for a specific target and Swift SDK distribution to Swift application developers.

## Cross-Compilation Triples

[Swift SDKs](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0387-cross-compilation-destinations.md)
touch on the topic of triples: `host, build and target`. This clarifies
how these triples interact during cross-compilation. For this build, it is essential to take 
the perspective of the Swift compiler rather than the Swift package or
application level described in [docs/BuildManifesto.md](https://github.com/swiftlang/swift/blob/main/docs/BuildManifesto.md)

The triple `host` describes the machine where the executable (here: compiler) is
running on, which is the development environment. The triple `target` describes the
machine, which output is generated for. The compiler running on `host` generates
output (here: cross-compiled Swift core libraries) for a `target`, which can be used
to put together `Swift SDKs` and cross-compile applications. The triple
`build`, which is the machine the compiler is built on, is not relevant for this
build and a precondition, as it would address building the Swift toolchain.

## Distribute and use the SDK
Upon finishing the cross-compile build of `Swift Core Libraries` and `Swift
standard library`, the [Swift SDK
Generator](https://github.com/swiftlang/swift-sdk-generator) is executed to
install them in Swift SDK format to be used with the Swift Toolchain. The Swift
application developer consumes the `Swift SDK` and does not need to know abou its 
 build process and generation.

