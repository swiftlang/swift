// RUN: %empty-directory(%t.mod)
// RUN: %swift -emit-module -o %t.mod/test.swiftmodule %s -parse-as-library -emit-module-doc-path %t.mod/test.swiftdoc
// RUN: %sourcekitd-test -req=doc-info -module test -- -I %t.mod | %FileCheck %s

public protocol Proto {}

public struct AttributesSlice1<T> : Proto {}

public struct ListFormatStyle<Style, Base: Proto> {
  public enum Width {
    case standard
    case short
    case narrow
  }
}

public extension Proto {
  // The tricky part about this test case is that when synthesizing this
  // extension for `AttributesSlice1`, we replace `Self` by
  // `AttributesSlice1<T>` but `S` remains an generic parameter. We thus
  // have a type that contains both an archetype (namely `T` as the generic
  // paramter of `AttributedSlice1<T>`) and an unbound generic paramters.
  // This used to cause issues when printing the type.
  func formatted<S>(width: ListFormatStyle<S, Self>.Width) -> String {
// CHECK: func formatted<S>(width width: ListFormatStyle<S, AttributesSlice1<T>>.Width) -> String
    fatalError()
  }
}

