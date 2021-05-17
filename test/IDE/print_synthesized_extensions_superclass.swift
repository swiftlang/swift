// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/print_synthesized_extensions_superclass.swiftmodule -emit-module-doc -emit-module-doc-path %t/print_synthesized_extensions_superclass.swiftdoc %s
// RUN: %target-swift-ide-test -print-module -synthesize-extension -print-interface -no-empty-line-between-members -module-to-print=print_synthesized_extensions_superclass -I %t -source-filename=%s | %FileCheck %s

public class Base {}
public class Middle<T> : Base {}
public class Most : Middle<Int> {}

public protocol P {
  associatedtype T
  associatedtype U
}

public extension P where T : Base {
  func withBase() {}
}

public extension P where T : Middle<U> {
  func withMiddleAbstract() {}
}

public extension P where T : Middle<Int> {
  func withMiddleConcrete() {}
}

public extension P where T : Most {
  func withMost() {}
}

// CHECK-LABEL: public struct S1 : print_synthesized_extensions_superclass.P {
// CHECK-NEXT:    public typealias T = print_synthesized_extensions_superclass.Base
// CHECK-NEXT:    public typealias U = Int
// CHECK-NEXT:    public func withBase()
// CHECk-NEXT:  }

public struct S1 : P {
  public typealias T = Base
  public typealias U = Int
}

// CHECK-LABEL: public struct S2 : print_synthesized_extensions_superclass.P {
// CHECK-NEXT:    public typealias T = print_synthesized_extensions_superclass.Middle<Int>
// CHECK-NEXT:    public typealias U = Int
// CHECK-NEXT:    public func withBase()
// CHECk-NEXT:    public func withMiddleAbstract()
// CHECk-NEXT:    public func withMiddleConcrete()
// CHECk-NEXT:  }

public struct S2 : P {
  public typealias T = Middle<Int>
  public typealias U = Int
}

// CHECK-LABEL: public struct S3 : print_synthesized_extensions_superclass.P {
// CHECK-NEXT:    public typealias T = print_synthesized_extensions_superclass.Middle<String>
// CHECK-NEXT:    public typealias U = String
// CHECK-NEXT:    public func withBase()
// CHECK-NEXT:    public func withMiddleAbstract()
// CHECK-NEXT:  }

public struct S3 : P {
  public typealias T = Middle<String>
  public typealias U = String
}

// CHECK-LABEL: public struct S4 : print_synthesized_extensions_superclass.P {
// CHECK-NEXT:    public typealias T = print_synthesized_extensions_superclass.Most
// CHECK-NEXT:    public typealias U = Int
// CHECK-NEXT:    public func withBase()
// CHECK-NEXT:    public func withMiddleAbstract()
// CHECK-NEXT:    public func withMiddleConcrete()
// CHECK-NEXT:    public func withMost()
// CHECK-NEXT:  }

public struct S4 : P {
  public typealias T = Most
  public typealias U = Int
}

// CHECK-LABEL: public struct S5 : print_synthesized_extensions_superclass.P {
// CHECK-NEXT:   public typealias T = print_synthesized_extensions_superclass.Most
// CHECK-NEXT:   public typealias U = String
// CHECK-NEXT:   public func withBase()
// CHECK-NEXT:   public func withMiddleConcrete()
// CHECK-NEXT:   public func withMost()
// CHECK-NEXT: }

public struct S5 : P {
  public typealias T = Most
  public typealias U = String
}

// CHECK-LABEL: public struct S6<T, U> : print_synthesized_extensions_superclass.P where T : print_synthesized_extensions_superclass.Base {
// CHECK-NEXT:    public func withBase()
// CHECK-NEXT:  }

// CHECK-LABEL: extension S6 where T : Middle<U> {
// CHECK-NEXT:    public func withMiddleAbstract()
// CHECK-NEXT:  }

// CHECK-LABEL: extension S6 where T : Middle<Int> {
// CHECK-NEXT:    public func withMiddleConcrete()
// CHECK-NEXT:  }

// CHECK-LABEL: extension S6 where T : Most {
// CHECK-NEXT:    public func withMost()
// CHECK-NEXT:  }

public struct S6<T, U> : P where T : Base {}

// CHECK-LABEL: public struct S7<T, U> : print_synthesized_extensions_superclass.P where T : print_synthesized_extensions_superclass.Middle<U> {
// CHECK-NEXT:    public func withBase()
// CHECK-NEXT:    public func withMiddleAbstract()
// CHECK-NEXT:  }

// CHECK-LABEL: extension S7 where T : Middle<Int> {
// CHECK-NEXT:    public func withMiddleConcrete()
// CHECK-NEXT:  }

// CHECK-LABEL: extension S7 where T : Most {
// CHECK-NEXT:    public func withMost()
// CHECK-NEXT:  }

public struct S7<T, U> : P where T : Middle<U> {}

// CHECK-LABEL: public struct S8<T, U> : print_synthesized_extensions_superclass.P where T : print_synthesized_extensions_superclass.Most {
// CHECK-NEXT:    public func withBase()
// CHECK-NEXT:    public func withMiddleConcrete()
// CHECK-NEXT:    public func withMost()
// CHECK-NEXT:  }

public struct S8<T, U> : P where T : Most {}