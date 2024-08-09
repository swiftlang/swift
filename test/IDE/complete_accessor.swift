// RUN: %batch-code-completion

// WITH_GETSET: Keyword/None:                       get; name=get
// WITH_GETSET: Keyword/None:                       set; name=set
// WITH_GETSET: Keyword/None:                       init; name=init
// NO_GETSET-NOT: get
// NO_GETSET-NOT: set

// WITH_OBSERVER: Keyword/None:                       willSet; name=willSet
// WITH_OBSERVER: Keyword/None:                       didSet; name=didSet
// NO_OBSERVER-NOT: willSet
// NO_OBSERVER-NOT: didSet

// WITH_GLOBAL: Decl[GlobalVar]/CurrModule{{(/TypeRelation\[Convertible\])?}}: globalValue[#String#];
// NO_GLOBAL-NOT: globalValue;

// WITH_SELF: Decl[LocalVar]/Local:               self[#{{.+}}#]; name=self
// NO_SELF-NOT: self

var globalValue: String

var something1: String = 1 {
  #^GLOBAL_FIRST?check=WITH_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
  willSet {}
}

var something2: String {
  get {}
  #^GLOBAL_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
}

func testLocal() {
  var something3: String = 1 {
    #^LOCAL_FIRST?check=WITH_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
    willSet {}
  }

  var something4: String {
    get {}
    #^LOCAL_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
  }
}

protocol SomeProto {
  var prop1: Int {
    #^PROTOCOL_PROPERTY_FIRST?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  var prop2: Int {
    get #^PROTOCOL_PROPERTY_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  subscript(_1 index:Int) -> Int {
    #^PROTOCOL_SUBSCRIPT_FIRST?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  subscript(_2 index:Int) -> String {
    get
    #^PROTOCOL_SUBSCRIPT_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
}

extension SomeProto {
  var prop1: Int {
    #^PROTOCOL_EXT_PROPERTY_FIRST?check=WITH_GLOBAL;check=WITH_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  var prop2: Int {
    set {} #^PROTOCOL_EXT_PROPERTY_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  subscript(_1 index:Int) -> Int {
    #^PROTOCOL_EXT_SUBSCRIPT_FIRST?check=WITH_GLOBAL;check=WITH_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  subscript(_2 index:Int) -> String {
    get { }
    #^PROTOCOL_EXT_SUBSCRIPT_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
}

struct SomeStruct {
  var prop1: Int {
    #^CONCRETE_PROPERTY_FIRST?check=WITH_GLOBAL;check=WITH_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
  }
  var prop2: Int {
    get {}
    @available(*, unavailable)
    #^CONCRETE_PROPERTY_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
  }
  subscript<T>(_1 index: T) -> Int {
    #^CONCRETE_SUBSCRIPT_FIRST?check=WITH_GLOBAL;check=WITH_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  subscript(_2 index: Int) -> String {
    get { }
    #^CONCRETE_SUBSCRIPT_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
}

extension SomeStruct {
  var prop3: Int {
    #^CONCRETE_EXT_PROPERTY_FIRST?check=WITH_GLOBAL;check=WITH_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
  }
  var prop4: Int {
    get {}
    #^CONCRETE_EXT_PROPERTY_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
  }
  subscript(_3 index:Int) -> Int {
    #^CONCRETE_EXT_SUBSCRIPT_FIRST?check=WITH_GLOBAL;check=WITH_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  subscript<U>(_4 index: Int) -> U {
    get { }
    #^CONCRETE_EXT_SUBSCRIPT_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
}

extension UNKNOWN_TYPE {
  var prop1: Int {
    #^UNKNOWN_EXT_PROPERTY_FIRST?check=WITH_GLOBAL;check=WITH_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
  }
  var prop2: Int {
    get {}
    #^UNKNOWN_EXT_PROPERTY_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=WITH_OBSERVER^#
  }
  subscript<T>(_1 index: T) -> T where T: ANOTHER_UNKNOWN_TYPE {
    #^UNKNOWN_EXT_SUBSCRIPT_FIRST?check=WITH_GLOBAL;check=WITH_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
  subscript(_2 index: Int) -> String {
    get { }
    #^UNKNOWN_EXT_SUBSCRIPT_SECOND?check=NO_GLOBAL;check=NO_SELF;check=WITH_GETSET;check=NO_OBSERVER^#
  }
}
