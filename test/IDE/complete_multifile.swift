// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-ide-test -code-completion -source-filename %t/Main.swift %t/Library.swift -code-completion-token=POINT_PAREN | %FileCheck --check-prefix=POINT_PAREN %s
// RUN: %target-swift-ide-test -code-completion -source-filename %t/Main.swift %t/Library.swift -code-completion-token=POINT_DOT | %FileCheck --check-prefix=POINT_DOT %s
// RUN: %target-swift-ide-test -code-completion -source-filename %t/Main.swift %t/Library.swift -code-completion-token=LENS_DOT | %FileCheck --check-prefix=LENS_DOT %s
// RUN: %target-swift-ide-test -code-completion -source-filename %t/Main.swift %t/Library.swift -code-completion-token=MYENUM_DOT | %FileCheck --check-prefix=MYENUM_DOT %s
// RUN: %target-swift-ide-test -code-completion -source-filename %t/Main.swift %t/Library.swift -code-completion-token=HASWRAPPED_DOT| %FileCheck --check-prefix=HASWRAPPED_DOT %s

// BEGIN Library.swift

public struct Point {
  var x: Int
  var y: Int
}

@dynamicMemberLookup
struct Lens<T> {
  var obj: T
  init(_ obj: T) { self.obj = obj }

  subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> Lens<U> {
    get { return Lens<U>(obj[keyPath: member]) }
    set { obj[keyPath: member] = newValue.obj }
  }
}

enum MyEnum: String, CaseIterable {
  case foo = "foo"
  case bar = "bar"
}

@propertyWrapper
struct Wrap<T> {
  var wrappedValue: T

  public var projectedValue: Self {
    get { self }
    set { self = newValue }
  }
}

struct HasWrapped {
  @Wrap
  var wrapped: Int = 1
}

// BEGIN Main.swift

func testStructDefaultInit() {
  Point(#^POINT_PAREN^#
// POINT_PAREN: Begin completions, 1 items
// POINT_PAREN-DAG: Decl[Constructor]/CurrNominal:      ['(']{#x: Int#}, {#y: Int#}[')'][#Point#];
// POINT_PAREN: End completions
  func sync() {}
  Point.#^POINT_DOT^#
// POINT_DOT: Begin completions, 3 items
// POINT_DOT-DAG: Keyword[self]/CurrNominal:          self[#Point.Type#];
// POINT_DOT-DAG: Keyword/CurrNominal:                Type[#Point.Type#];
// POINT_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#x: Int#}, {#y: Int#})[#Point#];
// POINT_DOT: End completions
}

func testDynamicMemberLookup(lens: Lens<Point>) {
  _ = lens.#^LENS_DOT^#
// LENS_DOT: Begin completions, 4 items
// LENS_DOT-DAG: Keyword[self]/CurrNominal:          self[#Lens<Point>#];
// LENS_DOT-DAG: Decl[InstanceVar]/CurrNominal:      x[#Lens<Int>#];
// LENS_DOT-DAG: Decl[InstanceVar]/CurrNominal:      y[#Lens<Int>#];
// LENS_DOT-DAG: Decl[InstanceVar]/CurrNominal:      obj[#Point#];
// LENS_DOT: End completions
}
func testRawRepresentable() {
  MyEnum.#^MYENUM_DOT^#
// MYENUM_DOT: Begin completions, 9 items
// MYENUM_DOT-DAG: Keyword[self]/CurrNominal:          self[#MyEnum.Type#];
// MYENUM_DOT-DAG: Keyword/CurrNominal:                Type[#MyEnum.Type#];
// MYENUM_DOT-DAG: Decl[EnumElement]/CurrNominal:      foo[#MyEnum#];
// MYENUM_DOT-DAG: Decl[EnumElement]/CurrNominal:      bar[#MyEnum#];
// MYENUM_DOT-DAG: Decl[TypeAlias]/CurrNominal:        RawValue[#String#];
// MYENUM_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#rawValue: String#})[#MyEnum?#];
// MYENUM_DOT-DAG: Decl[TypeAlias]/CurrNominal:        AllCases[#[MyEnum]#];
// MYENUM_DOT-DAG: Decl[StaticVar]/CurrNominal:        allCases[#[MyEnum]#];
// MYENUM_DOT-DAG: Decl[InstanceMethod]/Super:         hash({#(self): MyEnum#})[#(into: inout Hasher) -> Void#];
// MYENUM_DOT: End completions
}

func testHasWrappedValue(value: HasWrapped) {
  value.#^HASWRAPPED_DOT^#
// HASWRAPPED_DOT: Begin completions, 3 items
// HASWRAPPED_DOT: Keyword[self]/CurrNominal:          self[#HasWrapped#];
// HASWRAPPED_DOT: Decl[InstanceVar]/CurrNominal:      wrapped[#Int#];
// HASWRAPPED_DOT: Decl[InstanceVar]/CurrNominal:      $wrapped[#Wrap<Int>#];
// HASWRAPPED_DOT: End completions
}
