// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s

public enum Foo {}
// CHECK: <attr-builtin>public</attr-builtin> <kw>enum</kw> Foo {}
public extension Foo {
// CHECK: <attr-builtin>public</attr-builtin> <kw>extension</kw> <type>Foo</type> {
	protocol Bar {}
// CHECK: <kw>protocol</kw> Bar {}
}

public enum Result {
// CHECK: <attr-builtin>public</attr-builtin> <kw>enum</kw> Result {
  case success(a b = {
// CHECK: <kw>case</kw> success(a b = {

@available(*)
// CHECK: <attr-builtin>@available</attr-builtin>(*)
func materialize
// CHECK: <kw>func</kw> materialize