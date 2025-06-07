// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi-before.json %s -enable-library-evolution -DBASELINE -emit-tbd-path %t/abi-before.tbd -tbd-install_name Foo
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-abi-descriptor-path %t/abi-after.json %s -enable-library-evolution -emit-tbd-path %t/abi-after.tbd -tbd-install_name Foo
// RUN: %api-digester -diagnose-sdk --input-paths %t/abi-before.json -input-paths %t/abi-after.json -abi -o %t/result.txt
// RUN: %FileCheck %s < %t/result.txt

#if BASELINE

public class C {
	public func foo() {}
}

#else

public class C {}
extension C {
	public func foo() {}
}

#endif

// CHECK: Non-final class member Func C.foo() is moved to extension
