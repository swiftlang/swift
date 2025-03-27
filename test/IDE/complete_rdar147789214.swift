// RUN: %batch-code-completion

// rdar://147789214 - Make sure we insert generic parameters for underlying type.

struct S<T> {}
typealias Foo = S
typealias Bar = Foo
typealias Baz<T> = S<T>
typealias Invalid = (S, S)

let _: S = #^COMPLETE^#
// COMPLETE-DAG: Decl[TypeAlias]/CurrModule: Foo[#S<T>#]; name=Foo
// COMPLETE-DAG: Decl[TypeAlias]/CurrModule: Bar[#Foo#]; name=Bar
// COMPLETE-DAG: Decl[TypeAlias]/CurrModule: Baz[#S<T>#]; name=Baz
// COMPLETE-DAG: Decl[TypeAlias]/CurrModule: Invalid[#Invalid#]; name=Invalid

struct R<U> {
  typealias X = S<U>
  typealias Y = S
  typealias Z<T> = S<T>

  func foo() {
    // TODO: Once we start comparing type relations for types with generic
    // parameters, ideally 'Y' and 'Z' should be convertible, but not 'X'.
    let _: S<Int> = #^COMPLETE_IN_TYPE^#
    // COMPLETE_IN_TYPE-DAG: Decl[TypeAlias]/CurrNominal: X[#S<U>#]; name=X
    // COMPLETE_IN_TYPE-DAG: Decl[TypeAlias]/CurrNominal: Y[#S<T>#]; name=Y
    // COMPLETE_IN_TYPE-DAG: Decl[TypeAlias]/CurrNominal: Z[#S<T>#]; name=Z
  }
}
