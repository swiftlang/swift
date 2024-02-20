// RUN: %batch-code-completion

func test1(x: #^PARAM?check=HAS_SOME_ANY;check=HAS_REPEAT^#) {}

func test2() -> #^RESULT?check=HAS_SOME_ANY;check=NO_REPEAT^# {}

func test3() {
  // FIXME: 'repeat' is not valid here semantically but we allow it syntactically and can't (easily) tell in the parser whether 'repeat' is valid. Not worth fixing in the old parser.
  let a: [#^ARRAY_TYPE?check=HAS_SOME_ANY;check=HAS_REPEAT^#]
  let b: any #^AFTER_ANY?check=NO_SOME_ANY;check=NO_REPEAT^#
}

func test4(x: repeat #^AFTER_REPEAT?check=HAS_SOME_ANY;check=NO_REPEAT^#) {}

// HAS_SOME_ANY-DAG: Keyword/None: some; name=some
// HAS_SOME_ANY-DAG: Keyword/None: any; name=any
// HAS_SOME_ANY-DAG: Keyword/None: each; name=each

// NO_SOME_ANY-NOT: Keyword/None: some; name=some
// NO_SOME_ANY-NOT: Keyword/None: any; name=any
// NO_SOME_ANY-NOT: Keyword/None: each; name=each

// HAS_REPEAT-DAG: Keyword/None: repeat; name=repeat

// NO_REPEAT-NOT: Keyword/None: repeat; name=repeat
