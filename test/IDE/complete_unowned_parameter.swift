// RUN: %batch-code-completion

// UNOWNED_PARAMETER-DAG: Keyword/None:                       safe; name=safe
// UNOWNED_PARAMETER-DAG: Keyword/None:                       unsafe; name=unsafe

unowned(#^UNOWNED_TOP_LEVEL?check=UNOWNED_PARAMETER^#) var count = 0

struct MyStruct {
  unowned(#^UNOWNED_IN_STRUCT?check=UNOWNED_PARAMETER^#) var prop: Int = 0
}
