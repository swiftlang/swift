// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=default %s | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-%target-cpu

// REQUIRES: OS=windows-msvc

import InRegSRet

final public class BasicBlock {
}

extension OptionalBridgedBasicBlock {
  public var block: BasicBlock? { nil }
}

final public class Function {
  public var bridged: BridgedFunction {
    BridgedFunction()
  }

  public var firstBlock : BasicBlock? { bridged.getFirstBlock().block }
}

// Check that inreg on the sret isn't missing

// CHECK-x86_64: call void @"?getFirstBlock@BridgedFunction@@QEBA?AUOptionalBridgedBasicBlock@@XZ"(ptr {{.*}}, ptr noalias{{( nocapture)?}} sret(%TSo25OptionalBridgedBasicBlockV){{( captures\(none\))?}} {{.*}})
// CHECK-aarch64: call void @"?getFirstBlock@BridgedFunction@@QEBA?AUOptionalBridgedBasicBlock@@XZ"(ptr {{.*}}, ptr inreg noalias{{( nocapture)?}} sret(%TSo25OptionalBridgedBasicBlockV){{( captures\(none\))?}} {{.*}})

// CHECK-x86_64: define {{.*}} void @"?getFirstBlock@BridgedFunction@@QEBA?AUOptionalBridgedBasicBlock@@XZ"(ptr {{.*}} %{{.*}}, ptr {{.*}} sret(%struct.OptionalBridgedBasicBlock) {{.*}} %{{.*}})
// CHECK-aarch64: define {{.*}} void @"?getFirstBlock@BridgedFunction@@QEBA?AUOptionalBridgedBasicBlock@@XZ"(ptr {{.*}} %{{.*}}, ptr inreg {{.*}} sret(%struct.OptionalBridgedBasicBlock) {{.*}} %{{.*}})
