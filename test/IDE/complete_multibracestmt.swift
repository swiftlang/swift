// RUN: %batch-code-completion

enum E {
  case bar
  case foo
}

func test(pred: Bool) {
    var e: E
    if pred {
        e = .#^THEN?check=CHECK^#
    } else {
        e = .#^ELEE?check=CHECK^#
    }

    do {
        e = .#^DO?check=CHECK^#
    } catch e {
        e = .#^CATCH?check=CHECK^#
    }

    switch pred {
    case true:
        e = .#^CASE_1?check=CHECK^#
        break;
    case false:
        e = .#^CASE_2?check=CHECK^#
        break;
    default:
        e = .#^DEFAULT?check=CHECK^#
        break;
    }
}

// CHECK: Begin completions, 3 items
// CHECK-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: bar[#E#]; name=bar
// CHECK-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: foo[#E#]; name=foo
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): E#})[#(into: inout Hasher) -> Void#]; name=hash(:)
