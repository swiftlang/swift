// RUN: %swift-ide-test -code-completion -code-completion-annotate-results -source-filename %s -code-completion-token=GLOBAL_EXPR | %FileCheck %s --check-prefix=GLOBAL_EXPR
// RUN: %swift-ide-test -code-completion -code-completion-annotate-results -source-filename %s -code-completion-token=GLOBAL_EXPR | %FileCheck %s --check-prefix=GLOBAL_EXPR
// NOTE: To GLOBAL_EXPR twice to test completion from the cache.
// RUN: %swift-ide-test -code-completion -code-completion-annotate-results -source-filename %s -code-completion-token=GLOBAL_TYPE | %FileCheck %s --check-prefix=GLOBAL_TYPE
// RUN: %swift-ide-test -code-completion -code-completion-annotate-results -source-filename %s -code-completion-token=EXPR_MEMBER | %FileCheck %s --check-prefix=EXPR_MEMBER
// RUN: %swift-ide-test -code-completion -code-completion-annotate-results -source-filename %s -code-completion-token=EXPR_POSTFIX | %FileCheck %s --check-prefix=EXPR_POSTFIX
// RUN: %swift-ide-test -code-completion -code-completion-annotate-results -source-filename %s -code-completion-token=EXPR_IMPLICITMEMBER | %FileCheck %s --check-prefix=EXPR_IMPLICITMEMBER
// RUN: %swift-ide-test -code-completion -code-completion-annotate-results -source-filename %s -code-completion-token=CALLARG | %FileCheck %s --check-prefix=CALLARG

struct MyStruct {
  init(x: Int) {}
  var propNormal: Int { fatalError() }
  var propFunction: () -> MyStruct { fatalError() }
  func labelNameParamName(label param: (inout Int) throws -> MyStruct) rethrows {}
  func labelName(label: (@autoclosure () -> Int) -> Int) {}
  func sameName(label label: inout Int) {}
  func paramName(_ param: Int) {}
  subscript(param: Int) -> Int { 1 }
  subscript(label param: Int) -> Int { 1 }

  static func +(lhs: MyStruct, rhs: MyStruct) -> MyStruct { fatalError() }

  static var instance: MyStruct { fatalError() }
  static func create(x: Int) -> MyStruct { fatalError() }
}

func testGlobal() {
  #^GLOBAL_EXPR^#
}
// GLOBAL_EXPR: Begin completions
// GLOBAL_EXPR-DAG: Decl[Struct]/CurrModule:            <name>MyStruct</name>; typename=<typeid.user>MyStruct</typeid.user>;
// GLOBAL_EXPR-DAG: Keyword[class]/None:                <keyword>class</keyword>; typename=;
// GLOBAL_EXPR-DAG: Keyword[enum]/None:                 <keyword>enum</keyword>; typename=;
// GLOBAL_EXPR-DAG: Keyword[if]/None:                   <keyword>if</keyword>; typename=;
// GLOBAL_EXPR-DAG: Keyword[guard]/None:                <keyword>guard</keyword>; typename=;
// GLOBAL_EXPR-DAG: Keyword[try]/None:                  <keyword>try</keyword>; typename=;
// GLOBAL_EXPR-DAG: Keyword[try]/None:                  <keyword>try!</keyword>; typename=;
// GLOBAL_EXPR-DAG: Keyword/None:                       <keyword>Any</keyword>; typename=<keyword>Any</keyword>;
// GLOBAL_EXPR-DAG: Literal[Integer]/None:              0; typename=<typeid.sys>Int</typeid.sys>;
// GLOBAL_EXPR-DAG: Literal[Boolean]/None:              <name>true</name>; typename=<typeid.sys>Bool</typeid.sys>;
// GLOBAL_EXPR-DAG: Literal[Boolean]/None:              <name>false</name>; typename=<typeid.sys>Bool</typeid.sys>;
// GLOBAL_EXPR-DAG: Literal[Nil]/None:                  <name>nil</name>; typename=;
// GLOBAL_EXPR-DAG: Literal[String]/None:               &quot;<callarg><callarg.param>abc</callarg.param></callarg>&quot;; typename=<typeid.sys>String</typeid.sys>;
// GLOBAL_EXPR-DAG: Literal[Array]/None:                [<callarg><callarg.param>values</callarg.param></callarg>]; typename=<typeid.sys>Array</typeid.sys>;
// GLOBAL_EXPR-DAG: Literal[Dictionary]/None:           [<callarg><callarg.param>key</callarg.param></callarg>: <callarg><callarg.param>value</callarg.param></callarg>]; typename=<typeid.sys>Dictionary</typeid.sys>;
// GLOBAL_EXPR-DAG: Literal[_Color]/None:               <name>#colorLiteral</name>(<callarg><callarg.label>red</callarg.label>: <callarg.type><typeid.sys>Float</typeid.sys></callarg.type></callarg>, <callarg><callarg.label>green</callarg.label>: <callarg.type><typeid.sys>Float</typeid.sys></callarg.type></callarg>, <callarg><callarg.label>blue</callarg.label>: <callarg.type><typeid.sys>Float</typeid.sys></callarg.type></callarg>, <callarg><callarg.label>alpha</callarg.label>: <callarg.type><typeid.sys>Float</typeid.sys></callarg.type></callarg>); typename=;
// GLOBAL_EXPR-DAG: Literal[_Image]/None:               <name>#imageLiteral</name>(<callarg><callarg.label>resourceName</callarg.label>: <callarg.type><typeid.sys>String</typeid.sys></callarg.type></callarg>); typename=;
// GLOBAL_EXPR-DAG: Literal[Tuple]/None:                (<callarg><callarg.param>values</callarg.param></callarg>); typename=;
// GLOBAL_EXPR-DAG: Keyword[#function]/None:            <name>#function</name>; typename=<typeid.sys>String</typeid.sys>;
// GLOBAL_EXPR-DAG: Decl[Module]/None/IsSystem:         <name>Swift</name>; typename=Module;
// GLOBAL_EXPR-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: <name>Int</name>; typename=<typeid.sys>Int</typeid.sys>;
// GLOBAL_EXPR-DAG: Decl[FreeFunction]/OtherModule[Swift]/IsSystem: <name>print</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>items</callarg.param>: <callarg.type><keyword>Any</keyword></callarg.type>...</callarg>, <callarg><callarg.label>to</callarg.label> <callarg.param>output</callarg.param>: &amp;<callarg.type><typeid.sys>TextOutputStream</typeid.sys></callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>;
// GLOBAL_EXPR: End completions


func testType(value: #^GLOBAL_TYPE^#) {}
// GLOBAL_TYPE: Begin completions
// GLOBAL_TYPE-DAG: Keyword/None:                       <keyword>Any</keyword>; typename=<keyword>Any</keyword>;
// GLOBAL_TYPE-DAG: Decl[Struct]/CurrModule:            <name>MyStruct</name>; typename=<typeid.user>MyStruct</typeid.user>;
// GLOBAL_TYPE-DAG: Decl[Module]/None:                  <name>swift_ide_test</name>; typename=Module;
// GLOBAL_TYPE-DAG: Decl[Module]/None/IsSystem:         <name>Swift</name>; typename=Module;
// GLOBAL_TYPE-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: <name>Int</name>; typename=<typeid.sys>Int</typeid.sys>;
// GLOBAL_TYPE: End completions


func testMember(value: MyStruct) {
  value.#^EXPR_MEMBER^#
}
// EXPR_MEMBER: Begin completions, 7 items
// EXPR_MEMBER-DAG: Keyword[self]/CurrNominal:          <keyword>self</keyword>; typename=<typeid.user>MyStruct</typeid.user>;
// EXPR_MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      <name>propNormal</name>; typename=<typeid.sys>Int</typeid.sys>;
// EXPR_MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      <name>propFunction</name>; typename=() -&gt; <typeid.user>MyStruct</typeid.user>;
// EXPR_MEMBER-DAG: Decl[InstanceMethod]/CurrNominal: <name>labelNameParamName</name>(<callarg><callarg.label>label</callarg.label> <callarg.param>param</callarg.param>: <callarg.type>(<keyword>inout</keyword> <typeid.sys>Int</typeid.sys>) <keyword>throws</keyword> -&gt; <typeid.user>MyStruct</typeid.user></callarg.type></callarg>) <keyword>rethrows</keyword>; typename=<typeid.sys>Void</typeid.sys>;
// EXPR_MEMBER-DAG: Decl[InstanceMethod]/CurrNominal: <name>labelName</name>(<callarg><callarg.label>label</callarg.label>: <callarg.type>(<attribute>@autoclosure</attribute> () -&gt; <typeid.sys>Int</typeid.sys>) -&gt; <typeid.sys>Int</typeid.sys></callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>;
// EXPR_MEMBER-DAG: Decl[InstanceMethod]/CurrNominal: <name>sameName</name>(<callarg><callarg.label>label</callarg.label>: &amp;<callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>;
// EXPR_MEMBER-DAG: Decl[InstanceMethod]/CurrNominal: <name>paramName</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>param</callarg.param>: <callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>;
// EXPR_MEMBER: End completions

func testPostfix(value: MyStruct) {
  value #^EXPR_POSTFIX^#
}
// EXPR_POSTFIX: Begin completions, 10 items
// EXPR_POSTFIX-DAG: Decl[InstanceVar]/CurrNominal:      <name>propNormal</name>; typename=<typeid.sys>Int</typeid.sys>;
// EXPR_POSTFIX-DAG: Decl[InstanceVar]/CurrNominal:      <name>propFunction</name>; typename=() -&gt; <typeid.user>MyStruct</typeid.user>;
// EXPR_POSTFIX-DAG: Decl[InstanceMethod]/CurrNominal:   <name>labelNameParamName</name>(<callarg><callarg.label>label</callarg.label> <callarg.param>param</callarg.param>: <callarg.type>(<keyword>inout</keyword> <typeid.sys>Int</typeid.sys>) <keyword>throws</keyword> -&gt; <typeid.user>MyStruct</typeid.user></callarg.type></callarg>) <keyword>rethrows</keyword>; typename=<typeid.sys>Void</typeid.sys>;
// EXPR_POSTFIX-DAG: Decl[InstanceMethod]/CurrNominal:   <name>labelName</name>(<callarg><callarg.label>label</callarg.label>: <callarg.type>(<attribute>@autoclosure</attribute> () -&gt; <typeid.sys>Int</typeid.sys>) -&gt; <typeid.sys>Int</typeid.sys></callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>;
// EXPR_POSTFIX-DAG: Decl[InstanceMethod]/CurrNominal:   <name>sameName</name>(<callarg><callarg.label>label</callarg.label>: &amp;<callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>;
// EXPR_POSTFIX-DAG: Decl[InstanceMethod]/CurrNominal:   <name>paramName</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>param</callarg.param>: <callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>); typename=<typeid.sys>Void</typeid.sys>;
// EXPR_POSTFIX-DAG: Decl[Subscript]/CurrNominal:        [<callarg><callarg.label>_</callarg.label> <callarg.param>param</callarg.param>: <callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>]; typename=<typeid.sys>Int</typeid.sys>;
// EXPR_POSTFIX-DAG: Decl[Subscript]/CurrNominal:        [<callarg><callarg.label>label</callarg.label> <callarg.param>param</callarg.param>: <callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>]; typename=<typeid.sys>Int</typeid.sys>;
// EXPR_POSTFIX-DAG: Keyword[self]/CurrNominal:          <keyword>self</keyword>; typename=<typeid.user>MyStruct</typeid.user>;
// EXPR_POSTFIX-DAG: Decl[InfixOperatorFunction]/OtherModule[Swift]/IsSystem: <name>+</name>; typename=<typeid.user>MyStruct</typeid.user>;
// EXPR_POSTFIX: End completions

func testImplicitMember() -> MyStruct {
  return .#^EXPR_IMPLICITMEMBER^#
}
// EXPR_IMPLICITMEMBER: Begin completions, 7 items
// EXPR_IMPLICITMEMBER-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]: <name>init</name>(<callarg><callarg.label>x</callarg.label>: <callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>); typename=<typeid.user>MyStruct</typeid.user>;
// EXPR_IMPLICITMEMBER-DAG: Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: <name>instance</name>; typename=<typeid.user>MyStruct</typeid.user>;
// EXPR_IMPLICITMEMBER-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: <name>labelNameParamName</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>self</callarg.param>: <callarg.type><typeid.user>MyStruct</typeid.user></callarg.type></callarg>); typename=(label: (<keyword>inout</keyword> <typeid.sys>Int</typeid.sys>) <keyword>throws</keyword> -&gt; <typeid.user>MyStruct</typeid.user>) -&gt; <typeid.sys>Void</typeid.sys>;
// EXPR_IMPLICITMEMBER-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: <name>labelName</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>self</callarg.param>: <callarg.type><typeid.user>MyStruct</typeid.user></callarg.type></callarg>); typename=(label: (<attribute>@autoclosure</attribute> () -&gt; <typeid.sys>Int</typeid.sys>) -&gt; <typeid.sys>Int</typeid.sys>) -&gt; <typeid.sys>Void</typeid.sys>;
// EXPR_IMPLICITMEMBER-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: <name>sameName</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>self</callarg.param>: <callarg.type><typeid.user>MyStruct</typeid.user></callarg.type></callarg>); typename=(label: <keyword>inout</keyword> <typeid.sys>Int</typeid.sys>) -&gt; <typeid.sys>Void</typeid.sys>;
// EXPR_IMPLICITMEMBER-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: <name>paramName</name>(<callarg><callarg.label>_</callarg.label> <callarg.param>self</callarg.param>: <callarg.type><typeid.user>MyStruct</typeid.user></callarg.type></callarg>); typename=(<typeid.sys>Int</typeid.sys>) -&gt; <typeid.sys>Void</typeid.sys>;
// EXPR_IMPLICITMEMBER-DAG: Decl[StaticMethod]/ExprSpecific/TypeRelation[Identical]: <name>create</name>(<callarg><callarg.label>x</callarg.label>: <callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>); typename=<typeid.user>MyStruct</typeid.user>;
// EXPR_IMPLICITMEMBER: End completions

func testArgument() -> MyStruct {
  func foo(x: Int, y: Int) {}
  foo(x: 1, #^CALLARG^#
}
// CALLARG: Begin completions, 1 items
// CALLARG-DAG: Pattern/ExprSpecific:               <callarg><callarg.label>y</callarg.label>: <callarg.type><typeid.sys>Int</typeid.sys></callarg.type></callarg>; typename=<typeid.sys>Int</typeid.sys>
// CALLARG: End completions

