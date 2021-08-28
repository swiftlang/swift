// RUN: %empty-directory(%t)

// RUN: %empty-directory(%t/Sources)
// RUN: %empty-directory(%t/Modules)
// RUN: cp %S/Inputs/complete_sourcefileinfo/MyModule1.swift %t/Sources/MyModule1.swift
// RUN: cp %S/Inputs/complete_sourcefileinfo/MyModule2.swift %t/Sources/MyModule2.swift
// RUN: %target-swiftc_driver -emit-module -o %t/Modules/MyModule.swiftmodule %t/Sources/MyModule1.swift %t/Sources/MyModule2.swift 
// RUN: test -f %t/Modules/MyModule.swiftsourceinfo


// RUN: %empty-directory(%/result)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t/result -code-completion-sourcefileinfo -I %t/Modules

// RUN: cp %S/Inputs/complete_sourcefileinfo/MyModule1-modified.swift %t/Sources/MyModule1.swift
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t/result -code-completion-sourcefileinfo -I %t/Modules

// RUN: cp %S/Inputs/complete_sourcefileinfo/MyModule2-modified.swift %t/Sources/MyModule2.swift
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-sourcefileinfo -I %t/Modules -code-completion-token=GLOBAL | %FileCheck %s --check-prefix GLOBAL_MOD
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-sourcefileinfo -I %t/Modules -code-completion-token=MEMBER | %FileCheck %s --check-prefix MEMBER_MOD

import MyModule

var globalValue: Int = 1

func test() {
  #^GLOBAL^#
}
// GLOBAL-LABEL: Known module source files
// GLOBAL-DAG: + {{.*}}{{[/\\]}}test{{[/\\]}}IDE{{[/\\]}}complete_sourcefileinfo.swift
// GLOBAL-DAG: + {{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// GLOBAL-DAG: + {{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule2.swift
// GLOBAL-LABEL: Begin completions
// GLOBAL-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: String[#String#]; name=String{{$}}
// GLOBAL-DAG: Decl[Struct]/OtherModule[MyModule]: MyStruct[#MyStruct#]; name=MyStruct; source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// GLOBAL-DAG: Decl[GlobalVar]/CurrModule: globalValue[#Int#]; name=globalValue; source={{.*}}{{[/\\]}}test{{[/\\]}}IDE{{[/\\]}}complete_sourcefileinfo.swift

// GLOBAL_MOD-LABEL: Known module source files
// GLOBAL_MOD-DAG: + {{.*}}{{[/\\]}}test{{[/\\]}}IDE{{[/\\]}}complete_sourcefileinfo.swift
// GLOBAL_MOD-DAG: + {{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// GLOBAL_MOD-DAG: - {{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule2.swift
// GLOBAL_MOD-LABEL: Begin completions
// GLOBAL_MOD-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: String[#String#]; name=String{{$}}
// GLOBAL_MOD-DAG: Decl[Struct]/OtherModule[MyModule]: MyStruct[#MyStruct#]; name=MyStruct; source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// GLOBAL_MOD-DAG: Decl[GlobalVar]/CurrModule: globalValue[#Int#]; name=globalValue; source={{.*}}{{[/\\]}}test{{[/\\]}}IDE{{[/\\]}}complete_sourcefileinfo.swift

func test(val: MyStruct) {
  val.#^MEMBER^#
}
// MEMBER-LABEL: Known module source files
// MEMBER-DAG: + {{.*}}{{[/\\]}}test{{[/\\]}}IDE{{[/\\]}}complete_sourcefileinfo.swift
// MEMBER-DAG: + {{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// MEMBER-DAG: + {{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule2.swift
// MEMBER-LABEL: Begin completions, 5 items
// MEMBER-DAG: Keyword[self]/CurrNominal:          self[#MyStruct#]; name=self{{$}}
// MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      propertyInType[#Int#]; name=propertyInType; source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal:   funcInType({#x: Int#})[#Void#]; name=funcInType(x:); source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      propertyInExtension[#String#]; name=propertyInExtension; source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule2.swift
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal:   funcInExtension()[#String#]; name=funcInExtension(); source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule2.swift
// MEMBER: End completions

// MEMBER_MOD-LABEL: Known module source files
// MEMBER_MOD-DAG: + {{.*}}{{[/\\]}}test{{[/\\]}}IDE{{[/\\]}}complete_sourcefileinfo.swift
// MEMBER_MOD-DAG: + {{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// MEMBER_MOD-DAG: - {{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule2.swift
// MEMBER_MOD-LABEL: Begin completions, 5 items
// MEMBER_MOD-DAG: Keyword[self]/CurrNominal:          self[#MyStruct#]; name=self{{$}}
// MEMBER_MOD-DAG: Decl[InstanceVar]/CurrNominal:      propertyInType[#Int#]; name=propertyInType; source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// MEMBER_MOD-DAG: Decl[InstanceMethod]/CurrNominal:   funcInType({#x: Int#})[#Void#]; name=funcInType(x:); source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule1.swift
// MEMBER_MOD-DAG: Decl[InstanceVar]/CurrNominal:      propertyInExtension[#String#]; name=propertyInExtension; source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule2.swift
// MEMBER_MOD-DAG: Decl[InstanceMethod]/CurrNominal:   funcInExtension()[#String#]; name=funcInExtension(); source={{.*}}{{[/\\]}}Sources{{[/\\]}}MyModule2.swift
// MEMBER_MOD: End completions
