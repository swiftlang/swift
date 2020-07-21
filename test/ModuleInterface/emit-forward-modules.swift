// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Foo.swiftmodule)
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/Foo.swiftmodule)
// RUN: echo "public func foo() {}" > %t/Foo.swift

import Foo

// Step 1: build swift interface from the source
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Foo -emit-module-interface-path %t/Foo.swiftmodule/%target-swiftinterface-name

// Step 2: building a module from the interface, and the module should not be a forwarding module.
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftmodule/%target-swiftinterface-name -o %t/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo

// RUN: not %{python} %S/ModuleCache/Inputs/check-is-forwarding-module.py %t/Foo.swiftmodule/%target-swiftmodule-name

// Step 3: given the adjacent binary module as a candidate, building a module from the interface should give us a forwarding module
// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftmodule/%target-swiftinterface-name -o %t/Foo-from-interface.swiftmodule -module-name Foo -candidate-module-file %t/Foo.swiftmodule/%target-swiftmodule-name

// RUN: %{python} %S/ModuleCache/Inputs/check-is-forwarding-module.py %t/Foo-from-interface.swiftmodule

// Step 4: given the stale adjacent binary module as a candidate, building a module from the interface should give us a binary module
// RUN: echo "// some comments" >> %t/Foo.swiftmodule/%target-swiftinterface-name

// RUN: %target-swift-frontend -compile-module-from-interface %t/Foo.swiftmodule/%target-swiftinterface-name -o %t/Foo-from-interface.swiftmodule -module-name Foo -candidate-module-file %t/Foo.swiftmodule/%target-swiftmodule-name

// RUN: not %{python} %S/ModuleCache/Inputs/check-is-forwarding-module.py %t/Foo-from-interface.swiftmodule
