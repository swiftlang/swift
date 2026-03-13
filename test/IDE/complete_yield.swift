// RUN: %batch-code-completion

struct YieldVariables {
  var stringValue: String

  var property: String {
    _read {
      yield #^IN_READ^#
      // IN_READ-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: stringValue[#String#]; name=stringValue
    }
    _modify {
      var localString = ""
      yield #^IN_MODIFY^#
      // IN_MODIFY-DAG: Decl[InstanceVar]/CurrNominal:      stringValue[#String#]; name=stringValue
      // IN_MODIFY-DAG: Decl[LocalVar]/Local:               localString[#String#]; name=localString
    }
  }

  var otherProperty: String {
    _modify {
      var localString = ""
      yield &#^IN_MODIFY_WITH_REF^#
      // FIXME: We should probably be returning a convertible type relation here.
      // IN_MODIFY_WITH_REF: Decl[LocalVar]/Local:               localString[#String#]; name=localString
      // IN_MODIFY_WITH_REF: Decl[InstanceVar]/CurrNominal:      stringValue[#String#]; name=stringValue
    }
  }
}

