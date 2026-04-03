# Swift Cross-Compiler Build

Opposed to Swift native compilation, this cross-compiles Swift core libraries to a specific target, which makes Swift a Cross-Compiler. It enables building a custom Swift `Cross-Compiler` for specific targets.

## Cross-Compilation Triples

[Swift SDKs for
    Cross-Compilation](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0387-cross-compilation-destinations.md)
touches on the topic of triples: `host, build and target`. This clarifies
how these triples interact during cross-compilation. It is essential to take 
the perspective of the Swift compiler rather than the Swift package or
application level described in [docs/BuildManifesto.md](https://github.com/swiftlang/swift/blob/main/docs/BuildManifesto.md)

The triple `host` describes the machine where the executable(here: compiler) is
running on, which is the the developer PC/CI. The triple `target` describes the
machine, which output is generated for. The compiler running on `host` generates
output(here: cross-compiled Swift core libraries) for a `target`, which can be used
to put together `Swift SDKs` and cross-compile applications. The triple
`build`, which is the machine the compiler is built on, is not relevant for this
build and a precondition, as it would address building the Swift toolchain.

## Distribute Cross-Compiled Swift as SDK
Upon finishing the `Cross-Compiler` build, the [Swift SDK
Generator](https://github.com/swiftlang/swift-sdk-generator) is
executed to install the `Cross-Compiler` to be used with the Swift
Toolchain. With this approach the Cross-Compiler only needs to be built
once and can then be distributed as a `SDK`.
The Swift application developer consumes the `SDK` and does not need to know about Cross-Compiler Build and Swift `SDK` generation.

