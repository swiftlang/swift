// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s | %FileCheck %s
var bar: AnyObject
var foo: AnyObject
foo = #^A^#
// CHECK: Decl[GlobalVar]/Local/TypeRelation[Identical]: foo[#AnyObject#];
