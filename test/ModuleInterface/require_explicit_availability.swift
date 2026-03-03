// -require-explicit-availability is not printed in interfaces, so no warnings
// should be emitted when verifying the interface.
// RUN: %target-swift-emit-module-interface(%t_require.swiftinterface) -require-explicit-availability=warn %s
// RUN: %target-swift-typecheck-module-from-interface(%t_require.swiftinterface) -verify

// -library-level=api implies -require-explicit-availability=warn and it _is_
// printed in the interface. Still, no diagnostics about required explicit
// availability should be emitted when verifying the interface.
// RUN: %target-swift-emit-module-interface(%t_api.swiftinterface) -library-level=api %s
// RUN: %target-swift-typecheck-module-from-interface(%t_api.swiftinterface) -verify

public struct NoAvailability { }
