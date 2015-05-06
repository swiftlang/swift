// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=true -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=SKIP1 %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=false -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=NOSKIP1 %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=true -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=INIT1 %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=true -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=INIT2 %s
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -skip-deinit=true -module-to-print=print_module_without_deinit -I %t -source-filename=%s | FileCheck -check-prefix=ATTR1 %s

// SKIP1: class PropertyOwnership {
// NOSKIP1: class PropertyOwnership {
class PropertyOwnership {
  // NOSKIP1-NEXT: deinit
  deinit {
  }
  // SKIP1-NEXT:    init()
  // NOSKIP1-NEXT:  init()

// SKIP1-NEXT:     }
// NOSKIP1-NEXT:   }
}

// INIT1: class OptionalInitContainer {
public class OptionalInitContainer {
  // INIT1: init?(){{$}}
  public init?() {
    return nil
  }
}

// INIT2: class ImplicitOptionalInitContainer {
public class ImplicitOptionalInitContainer {
  // INIT2: init!(){{$}}
  public init!() {
    return nil
  }
}

// ATTR1: class AttributeContainer1 {
public class AttributeContainer1 {
  // ATTR1: func m1(@autoclosure a: () -> Int)
  public func m1(@autoclosure a : ()->Int) {}
  // ATTR1: func m2(@noescape a: () -> Int)
  public func m2(@noescape a : ()->Int) {}
}
