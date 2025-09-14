# Embedded Swift language restrictions (EmbeddedRestrictions)

Embedded Swift is a compilation model of Swift that can produce extremely small binaries without external dependencies, suitable for restricted environments including embedded (microcontrollers) and baremetal setups (no operating system at all), and low-level environments (firmware, kernels, device drivers, low-level components of userspace OS runtimes). While the vast majority of Swift language features are available in Embedded Swift, there are some language features that require the full Swift standard library and runtime, which are not available in Embedded Swift.

Diagnostics in the `EmbeddedRestrictions` group describe those language features that cannot be used in Embedded Swift. For example, Embedded Swift uses a simplified reference-counting model that does not support `weak` or `unowned` references. The following will produce a diagnostic in Embedded Swift:

    class Node {
      weak var parent: Node?    // error: attribute 'weak' cannot be used in Embedded Swift
    }

## See Also

- [A Vision for Embedded Swift](https://github.com/swiftlang/swift-evolution/blob/main/visions/embedded-swift.md)
