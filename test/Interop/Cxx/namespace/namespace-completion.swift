// RUN: %target-swift-ide-test -code-completion -enable-experimental-cxx-interop -source-filename %s -code-completion-token=NS1 -I %S/Inputs | %FileCheck %s -check-prefix=CHECK-NS1
// RUN: %target-swift-ide-test -code-completion -enable-experimental-cxx-interop -source-filename %s -code-completion-token=NS2 -I %S/Inputs | %FileCheck %s -check-prefix=CHECK-NS2
// RUN: %target-swift-ide-test -code-completion -enable-experimental-cxx-interop -source-filename %s -code-completion-token=TemplatesNS1 -I %S/Inputs | %FileCheck %s -check-prefix=CHECK-TNS1
// RUN: %target-swift-ide-test -code-completion -enable-experimental-cxx-interop -source-filename %s -code-completion-token=EnumsNS1 -I %S/Inputs | %FileCheck %s -check-prefix=CHECK-ENS1

import Submodules
import Templates
import Enums

func ns1() {
  NS1.#^NS1^#
}
// CHECK-NS1: Begin completions, 5 items
// CHECK-NS1-NEXT: Keyword[self]/CurrNominal:          self[#NS1.Type#]; name=self
// CHECK-NS1-NEXT: Keyword/CurrNominal:                Type[#NS1.Type#]; name=Type
// CHECK-NS1-NEXT: Decl[Enum]/CurrNominal:             NS2[#NS1.NS2#]; name=NS2
// CHECK-NS1-NEXT: Decl[Struct]/CurrNominal:           BasicB[#NS1.BasicB#]; name=BasicB
// CHECK-NS1-NEXT: Decl[Struct]/CurrNominal:           BasicA[#NS1.BasicA#]; name=BasicA
// CHECK-NS1-NEXT: End completions

func ns2() {
  NS1.NS2.#^NS2^#
}

// CHECK-NS2: Begin completions, 4 items
// CHECK-NS2-NEXT: Keyword[self]/CurrNominal:          self[#NS1.NS2.Type#]; name=self
// CHECK-NS2-NEXT: Keyword/CurrNominal:                Type[#NS1.NS2.Type#]; name=Type
// CHECK-NS2-NEXT: Decl[Struct]/CurrNominal:           BasicDeepB[#NS1.NS2.BasicDeepB#]; name=BasicDeepB
// CHECK-NS2-NEXT: Decl[Struct]/CurrNominal:           BasicDeepA[#NS1.NS2.BasicDeepA#]; name=BasicDeepA
// CHECK-NS2-NEXT: End completions

func nsTemplates() {
  TemplatesNS1.#^TemplatesNS1^#
}

// CHECK-TNS1: Begin completions, 10 items
// CHECK-TNS1-NEXT: Keyword[self]/CurrNominal:          self[#TemplatesNS1.Type#]; name=self
// CHECK-TNS1-NEXT: Keyword/CurrNominal:                Type[#TemplatesNS1.Type#]; name=Type
// CHECK-TNS1-NEXT: Decl[StaticMethod]/CurrNominal:     basicFunctionTemplateDefinedInDefs({#T#})[#UnsafePointer<CChar>!#]; name=basicFunctionTemplateDefinedInDefs()
// CHECK-TNS1-NEXT: Decl[TypeAlias]/CurrNominal:        UseTemplate[#TemplatesNS4.HasSpecialization<CChar>#]; name=UseTemplate
// CHECK-TNS1-NEXT: Decl[TypeAlias]/CurrNominal:        UseSpecialized[#TemplatesNS4.HasSpecialization<CInt>#]; name=UseSpecialized
// CHECK-TNS1-NEXT: Decl[Enum]/CurrNominal:             TemplatesNS2[#TemplatesNS1.TemplatesNS2#]; name=TemplatesNS2
// CHECK-TNS1-NEXT: Decl[Enum]/CurrNominal:             TemplatesNS3[#TemplatesNS1.TemplatesNS3#]; name=TemplatesNS3
// CHECK-TNS1-NEXT: Decl[TypeAlias]/CurrNominal:        ForwardDeclaredClassTemplateChar[#TemplatesNS1.TemplatesNS2.ForwardDeclaredClassTemplate<CChar>#]; name=ForwardDeclaredClassTemplateChar
// CHECK-TNS1-NEXT: Decl[StaticMethod]/CurrNominal:     basicFunctionTemplate({#T#})[#UnsafePointer<CChar>!#]; name=basicFunctionTemplate()
// CHECK-TNS1-NEXT: Decl[TypeAlias]/CurrNominal:        BasicClassTemplateChar[#TemplatesNS1.BasicClassTemplate<CChar>#]; name=BasicClassTemplateChar
// CHECK-TNS1-NEXT: End completions

func nsEnums() {
  EnumNS1.#^EnumsNS1^#
}

// CHECK-ENS1: Keyword[self]/CurrNominal:          self[#EnumNS1.Type#]; name=self
// CHECK-ENS1-NEXT: Keyword/CurrNominal:                Type[#EnumNS1.Type#]; name=Type
// CHECK-ENS1-NEXT: Decl[Struct]/CurrNominal:           AnEnum[#EnumNS1.AnEnum#]; name=AnEnum
// CHECK-ENS1-NEXT: Decl[StaticVar]/CurrNominal:        one[#EnumNS1.AnEnum#]; name=one
// CHECK-ENS1-NEXT: Decl[StaticVar]/CurrNominal:        two[#EnumNS1.AnEnum#]; name=two
// CHECK-ENS1-NEXT: Decl[StaticVar]/CurrNominal:        three[#EnumNS1.AnEnum#]; name=three
// CHECK-ENS1-NEXT: End completions
