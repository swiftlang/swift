// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
enum A<h{
class a
protocol c{{
}func b
#^A^#
class b<T:e>:a
{}class e
