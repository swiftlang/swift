// RUN: %target-swift-frontend -emit-ir -enable-library-evolution %s | %FileCheck %s

public class ResilientClass {
  public dynamic func resilientMethod() {}
}

// The dynamic replacement key should be public even though the method
// implementation is hidden.

// CHECK-LABEL: @"$s36class_resilience_dynamic_replacement14ResilientClassC15resilientMethodyyFTx" = {{(dllexport |protected )?}}constant %swift.dyn_repl_key
// CHECK-LABEL: define hidden swiftcc void @"$s36class_resilience_dynamic_replacement14ResilientClassC15resilientMethodyyF"
