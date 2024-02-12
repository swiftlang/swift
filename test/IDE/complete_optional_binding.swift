// RUN: %batch-code-completion

let topLevelOpt: Int?

do {
  let topLevelLocalOpt: Int?
  let topLevelLocalNonOpt: Int

  if let #^TOPLEVEL_IF_LET?check=TOPLEVEL^#
// TOPLEVEL: Begin completions, 1 items
// TOPLEVEL-DAG: Decl[LocalVar]/Local:               topLevelLocalOpt[#Int?#];
// FIXME: show 'topLevelOpt'
}

struct MyStruct<T> {
    var propOpt: Int?
    var propNonOpt: Int 
    var propGenOpt: T?
    var propGenNonOpt: T

    func testMethod<U>(paramGenOpt: U?, paramGenNonOpt: U, paramOpt: Int?, paramNonOpt: Int) {
        var localOpt: Int?
        var localNonOpt: Int
        var localGenOpt: U?
        var localGenNonOpt: U

        do {
          if let #^IF_LET?check=IN_FUNC^#
        }
        do {
          if var #^IF_VAR?check=IN_FUNC^#
        }
        do {
          if true {} else if let #^ELSEIF_LET?check=IN_FUNC^#
        }
        do {
          if true {} else if var #^ELSEIF_VAR?check=IN_FUNC^#
        }
        do {
          guard let #^GUARD_LET?check=IN_FUNC^#
        }
        do {
          guard var #^GUARD_VAR?check=IN_FUNC^#
        }
        do {
          while let #^WHILE_LET?check=IN_FUNC^#
        }
        do {
          while var #^WHILE_VAR?check=IN_FUNC^#
        }

// IN_FUNC: Begin completions, 6 items
// IN_FUNC-DAG: Decl[LocalVar]/Local:               localOpt[#Int?#];
// IN_FUNC-DAG: Decl[LocalVar]/Local:               localGenOpt[#U?#];
// IN_FUNC-DAG: Decl[LocalVar]/Local:               paramGenOpt[#U?#];
// IN_FUNC-DAG: Decl[LocalVar]/Local:               paramOpt[#Int?#];
// IN_FUNC-DAG: Decl[InstanceVar]/CurrNominal:      propOpt[#Int?#];
// IN_FUNC-DAG: Decl[InstanceVar]/CurrNominal:      propGenOpt[#T?#];
// IN_FUNC-NOT: NonOpt
    }
}

func testPreviousElements() {
  let localOptOpt: Int??

  if let localOpt = localOptOpt, let localNonOpt = localOpt, let #^PREV_ELEMENT^#
// PREV_ELEMENT: Begin completions, 2 items
// PREV_ELEMENT-DAG: Decl[LocalVar]/Local:               localOptOpt[#Int??#];
// PREV_ELEMENT-DAG: Decl[LocalVar]/Local:               localOpt[#Int?#];
// PREV_ELEMENT-NOT: NonOpt
}
