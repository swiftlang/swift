// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: mkdir -p %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name SomeModule \
// RUN:     -o %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     %t/SomeModule.swift

// RUN: %target-swift-ide-test -code-completion -sdk %t/SDK -iframework %t/SDK/Frameworks -source-filename %t/main.swift -code-completion-token=GLOBAL | %FileCheck --check-prefix GLOBAL %s
// RUN: %target-swift-ide-test -code-completion -sdk %t/SDK -iframework %t/SDK/Frameworks -source-filename %t/main.swift -code-completion-token=GLOBAL_TYPE | %FileCheck --check-prefix GLOBAL_TYPE %s
// RUN: %target-swift-ide-test -code-completion -sdk %t/SDK -iframework %t/SDK/Frameworks -source-filename %t/main.swift -code-completion-token=INSTANCE | %FileCheck --check-prefix INSTANCE %s
// RUN: %target-swift-ide-test -code-completion -sdk %t/SDK -iframework %t/SDK/Frameworks -source-filename %t/main.swift -code-completion-token=INITIALIZER | %FileCheck --check-prefix INITIALIZER %s

// Test that declarations starting with '_' from system module doesn't apper in
// code completion.

// BEGIN SomeModule.swift

public struct SomeValue {
  internal var internalValue: Int { return 1 }
  public var _secretValue: Int { return 1 }
  public var publicValue: Int { return 1 }

  internal func internalMethod() -> Int { return 1 }
  public func _secretMethod() -> Int { return 1 }
  public func publicMethod() -> Int { return 1 }

  internal init(internal: Int) {}
  public init(_secret: Int) {}
  public init(public: Int) {}
}

internal func internalFunc() {}
public func _secretFunc() {}
public func publicFunc() {}

internal class InternalClass {}
public class _SecretClass {}
public class PublicClass {}

// BEGIN main.swift
import SomeModule

func test(value: SomeValue) {
  let _ = #^GLOBAL^#
// GLOBAL-NOT: _secretFunc
// GLOBAL-NOT: internalFunc
// GLOBAL-NOT: _SecretClass
// GLOBAL-NOT: InternalClass
// GLOBAL-DAG: Decl[Struct]/OtherModule[SomeModule]/IsSystem: SomeValue[#SomeValue#];
// GLOBAL-DAG: Decl[FreeFunction]/OtherModule[SomeModule]/IsSystem: publicFunc()[#Void#];
// GLOBAL-DAG: Decl[Class]/OtherModule[SomeModule]/IsSystem: PublicClass[#PublicClass#]; name=PublicClass

  let _: #^GLOBAL_TYPE^#
// GLOBAL_TYPE-NOT: InternalClass
// GLOBAL_TYPE-NOT: _SecretClass
// GLOBAL-TYPE-DAG: Decl[Struct]/OtherModule[SomeModule]: SomeValue[#SomeValue#];
// GLOBAL-TYPE-DAG: Decl[Class]/OtherModule[SomeModule]: PublicClass[#PublicClass#];

  let _ = value.#^INSTANCE^#
// INSTANCE: Begin completions, 3 items
// INSTANCE-DAG: Keyword[self]/CurrNominal:          self[#SomeValue#];
// INSTANCE-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: publicValue[#Int#];
// INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal/IsSystem: publicMethod()[#Int#];

  let _ = SomeValue(#^INITIALIZER^#
// INITIALIZER: Begin completions, 1 items
// INITIALIZER-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]/IsSystem: ['(']{#public: Int#}[')'][#SomeValue#];
}
