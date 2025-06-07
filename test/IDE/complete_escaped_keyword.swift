// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STATIC_PRIMARY | %FileCheck %s -check-prefix=STATIC_PRIMARY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STATIC_SELF_NODOT | %FileCheck %s -check-prefix=STATIC_SELF_NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STATIC_SELF_DOT | %FileCheck %s -check-prefix=STATIC_SELF_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=META_NODOT | %FileCheck %s -check-prefix=STATIC_SELF_NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=META_DOT | %FileCheck %s -check-prefix=STATIC_SELF_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_PRIMARY | %FileCheck %s -check-prefix=INSTANCE_PRIMARY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_SELF_NODOT | %FileCheck %s -check-prefix=INSTANCE_SELF_NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_SELF_DOT | %FileCheck %s -check-prefix=INSTANCE_SELF_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VALUE_NODOT | %FileCheck %s -check-prefix=INSTANCE_SELF_NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VALUE_DOT | %FileCheck %s -check-prefix=INSTANCE_SELF_DOT


enum MyEnum {
  case `class`(struct: String)
  case `let`(`var`: String)

  init(`init`: String) {}
  static func `public`(private: String) -> Int {}

  func `init`(deinit: String) -> Int {}
  func `if`(else: String) -> Int {}

  var `self`: Int { return 0 }

  static func testStatic(meta: MyEnum.Type) {
    let _ = #^STATIC_PRIMARY^#
// STATIC_PRIMARY: Begin completion
// STATIC_PRIMARY-DAG: Decl[LocalVar]/Local:               self[#MyEnum.Type#]; name=self
// STATIC_PRIMARY-DAG: Decl[EnumElement]/CurrNominal:      `class`({#struct: String#})[#MyEnum#]; name=`class`(struct:)
// STATIC_PRIMARY-DAG: Decl[EnumElement]/CurrNominal:      `let`({#var: String#})[#MyEnum#]; name=`let`(var:)
// STATIC_PRIMARY-DAG: Decl[StaticMethod]/CurrNominal:     `public`({#private: String#})[#Int#]; name=`public`(private:)
// STATIC_PRIMARY-DAG: Decl[InstanceMethod]/CurrNominal:   `init`({#(self): MyEnum#})[#(deinit: String) -> Int#]; name=`init`(:)
// STATIC_PRIMARY-DAG: Decl[InstanceMethod]/CurrNominal:   `if`({#(self): MyEnum#})[#(else: String) -> Int#]; name=`if`(:)
// STATIC_PRIMARY: End completion

    let _ = self#^STATIC_SELF_NODOT^#
// STATIC_SELF_NODOT-DAG: Keyword[self]/CurrNominal:          .self[#MyEnum.Type#]; name=self
// STATIC_SELF_NODOT-DAG: Decl[EnumElement]/CurrNominal:      .class({#struct: String#})[#MyEnum#]; name=class(struct:)
// STATIC_SELF_NODOT-DAG: Decl[EnumElement]/CurrNominal:      .let({#var: String#})[#MyEnum#]; name=let(var:)
// STATIC_SELF_NODOT-DAG: Decl[Constructor]/CurrNominal:      .init({#init: String#})[#MyEnum#]; name=init(init:)
// STATIC_SELF_NODOT-DAG: Decl[StaticMethod]/CurrNominal:     .public({#private: String#})[#Int#]; name=public(private:)
// STATIC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .`init`({#(self): MyEnum#})[#(deinit: String) -> Int#]; name=`init`(:)
// STATIC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .if({#(self): MyEnum#})[#(else: String) -> Int#]; name=if(:)
// STATIC_SELF_NODOT: End completion

    let _ = self.#^STATIC_SELF_DOT^#
// STATIC_SELF_DOT-DAG: Keyword[self]/CurrNominal:          self[#MyEnum.Type#]; name=self
// STATIC_SELF_DOT-DAG: Decl[EnumElement]/CurrNominal:      class({#struct: String#})[#MyEnum#]; name=class(struct:)
// STATIC_SELF_DOT-DAG: Decl[EnumElement]/CurrNominal:      let({#var: String#})[#MyEnum#]; name=let(var:)
// STATIC_SELF_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#init: String#})[#MyEnum#]; name=init(init:)
// STATIC_SELF_DOT-DAG: Decl[StaticMethod]/CurrNominal:     public({#private: String#})[#Int#]; name=public(private:)
// STATIC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   `init`({#(self): MyEnum#})[#(deinit: String) -> Int#]; name=`init`(:)
// STATIC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   if({#(self): MyEnum#})[#(else: String) -> Int#]; name=if(:)
// STATIC_SELF_DOT: End completion

    let _ = meta#^META_NODOT^#
// SAME AS 'STATIC_SELF_NODOT'.

    let _ = meta.#^META_DOT^#
// SAME AS 'STATIC_SELF_DOT'.
  }

  func testInstance(val: MyEnum) {
    let _ = #^INSTANCE_PRIMARY^#
// INSTANCE_PRIMARY: Begin completion
// INSTANCE_PRIMARY-NOT: self[#Int#];
// INSTANCE_PRIMARY-DAG: Decl[LocalVar]/Local:               self[#MyEnum#]; name=self
// INSTANCE_PRIMARY-DAG: Decl[InstanceMethod]/CurrNominal:   `init`({#deinit: String#})[#Int#]; name=`init`(deinit:)
// INSTANCE_PRIMARY-DAG: Decl[InstanceMethod]/CurrNominal:   `if`({#else: String#})[#Int#]; name=`if`(else:)
// INSTANCE_PRIMARY-NOT: self[#Int#];
// INSTANCE_PRIMARY: End completion

    let _ = self#^INSTANCE_SELF_NODOT^#
// INSTANCE_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .`init`({#deinit: String#})[#Int#]; name=`init`(deinit:)
// INSTANCE_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .if({#else: String#})[#Int#]; name=if(else:)
// INSTANCE_SELF_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .`self`[#Int#]; name=`self`
// INSTANCE_SELF_NODOT-DAG: Keyword[self]/CurrNominal:          .self[#MyEnum#]; name=self


    let _ = self.#^INSTANCE_SELF_DOT^#
// INSTANCE_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   `init`({#deinit: String#})[#Int#]; name=`init`(deinit:)
// INSTANCE_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   if({#else: String#})[#Int#]; name=if(else:)
// INSTANCE_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal:      `self`[#Int#]; name=`self`
// INSTANCE_SELF_DOT-DAG: Keyword[self]/CurrNominal:          self[#MyEnum#]; name=self

    let _ = val#^VALUE_NODOT^#
// SAME AS 'INSTANCE_SELF_NODOT'.
    let _ = val.#^VALUE_DOT^#
// SAME AS 'INSTANCE_SELF_DOT'.
  }
}
