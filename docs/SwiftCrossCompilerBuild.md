# Swift Cross-Compiler Build

Opposed to Swift native compilation, this cross-compiles Swift core libraries to a specific target, which makes Swift a Cross-Compiler. It enables building a custom Swift `Cross-Compiler` for specific targets.

## Cross-Compilation Triples

text here.

## Distribute Cross-Compiled Swift as SDK
Upon finishing the `Cross-Compiler` build, the [Swift SDK
Generator](https://github.com/swiftlang/swift-sdk-generator) is
executed to install the `Cross-Compiler` to be used with the Swift
Toolchain. With this approach the Cross-Compiler only needs to be built
once and can then be distributed as a `SDK`.
The Swift application developer consumes the `SDK` and does not need to know about Cross-Compiler Build and Swift `SDK` generation.

