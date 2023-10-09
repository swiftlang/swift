// RUN: %batch-code-completion

struct Test {
  var storedProperty: Int

  var attrBegin: Int {
    @#^ATTRIBUTE_BEGIN^# init(newValue) {
      print(newValue)
    }
    get { 1 }
  }
  // ATTRIBUTE_BEGIN: Keyword/None: storageRestrictions[#Accessor Attribute#];

  var bothLabels: Int {
    @storageRestrictions(#^AFTER_PAREN^#) 
    init(newValue) {
    }
    get { 1 }
  }
  // AFTER_PAREN: Begin completions, 2 items
  // AFTER_PAREN-DAG: Keyword/None: initializes: [#Specify stored properties initialized by the accessor#];
  // AFTER_PAREN-DAG: Keyword/None: accesses: [#Specify stored properties accessed by the accessor#];

  var secondAccessesArgument: Int {
    @storageRestrictions(accesses: x, #^SECOND_ACCESSES_ARGUMENT^#) 
    init(newValue) {
    }
  }
  // SECOND_ACCESSES_ARGUMENT: Begin completions, 2 items
  // SECOND_ACCESSES_ARGUMENT-DAG: Keyword/None: initializes: [#Specify stored properties initialized by the accessor#];
  // SECOND_ACCESSES_ARGUMENT-DAG: Decl[InstanceVar]/CurrNominal: storedProperty[#Int#];
  

  var secondInitializesArgument: Int {
    @storageRestrictions(initializes: x, #^SECOND_INITIALIZES_ARGUMENT^#) 
    init(newValue) {
    }
    get { 1 }
  }
  // SECOND_INITIALIZES_ARGUMENT: Begin completions, 2 items
  // SECOND_INITIALIZES_ARGUMENT-DAG: Keyword/None: accesses: [#Specify stored properties accessed by the accessor#];
  // SECOND_INITIALIZES_ARGUMENT-DAG: Decl[InstanceVar]/CurrNominal: storedProperty[#Int#];
}

struct TestArgument {
  var other: Int

  var otherComputed: Int { 1 }

  func testFunc() {}

  var firstInitializesArgument: Int {
    @storageRestrictions(initializes: #^FIRST_INITIALIZES_ARGUMENT?check=FIRST_ARGUMENT^#) 
    init(newValue) {
    }
    get { 1 }
  }
  // FIRST_ARGUMENT: Begin completions, 1 item
  // FIRST_ARGUMENT-DAG: Decl[InstanceVar]/CurrNominal: other[#Int#];

  var firstAccessesArgument: Int {
    @storageRestrictions(initializes: #^FIRST_ACCESSES_ARGUMENT?check=FIRST_ARGUMENT^#) 
    init(newValue) {
    }
    get { 1 }
  }
}
