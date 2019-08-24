// RUN: %target-typecheck-verify-swift

protocol P { }

func foo<T: P>(_: T) {}
func bar<T: P>(_: T.Type) {}

func open(existential: P, mutExistential: inout P) {
  _openExistential(existential, do: foo)
  _openExistential(type(of: existential), do: bar)
  _openExistential(mutExistential, do: foo)
  _openExistential(type(of: mutExistential), do: bar)
}
