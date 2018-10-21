// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_PROPERTY_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_PROPERTY_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_SUBSCRIPT_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_SUBSCRIPT_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_PROPERTY_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_PROPERTY_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_SUBSCRIPT_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_EXT_SUBSCRIPT_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONCRETE_PROPERTY_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONCRETE_PROPERTY_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONCRETE_SUBSCRIPT_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONCRETE_SUBSCRIPT_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONCRETE_EXT_PROPERTY_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONCRETE_EXT_PROPERTY_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONCRETE_EXT_SUBSCRIPT_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONCRETE_EXT_SUBSCRIPT_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNKNOWN_EXT_PROPERTY_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNKNOWN_EXT_PROPERTY_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNKNOWN_EXT_SUBSCRIPT_FIRST > %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNKNOWN_EXT_SUBSCRIPT_SECOND > %t.result
// RUN: %FileCheck %s -check-prefix=NO_GLOBAL < %t.result
// RUN: %FileCheck %s -check-prefix=NO_SELF < %t.result
// RUN: %FileCheck %s -check-prefix=WITH_GETSET < %t.result
// RUN: %FileCheck %s -check-prefix=NO_OBSERVER < %t.result

// WITH_GETSET: Keyword/None:                       get; name=get
// WITH_GETSET: Keyword/None:                       set; name=set
// NO_GETSET-NOT: get
// NO_GETSET-NOT: set

// WITH_OBSERVER: Keyword/None:                       willSet; name=willSet
// WITH_OBSERVER: Keyword/None:                       didSet; name=didSet
// NO_OBSERVER-NOT: willSet
// NO_OBSERVER-NOT: didSet

// WITH_GLOBAL: Decl[GlobalVar]/CurrModule:         globalValue[#String#];
// NO_GLOBAL-NOT: globalValue;

// WITH_SELF: Decl[LocalVar]/Local:               self[#{{.+}}#]; name=self
// NO_SELF-NOT: self

var globalValue: String

var something1: String = 1 {
  #^GLOBAL_FIRST^#
  willSet {}
}

var something2: String {
  get {}
  #^GLOBAL_SECOND^#
}

func testLocal() {
  var something3: String = 1 {
    #^LOCAL_FIRST^#
    willSet {}
  }

  var something4: String {
    get {}
    #^LOCAL_SECOND^#
  }
}

protocol SomeProto {
  var prop1: Int {
    #^PROTOCOL_PROPERTY_FIRST^#
  }
  var prop2: Int {
    get #^PROTOCOL_PROPERTY_SECOND^#
  }
  subscript(_1 index:Int) -> Int {
    #^PROTOCOL_SUBSCRIPT_FIRST^#
  }
  subscript(_2 index:Int) -> String {
    get
    #^PROTOCOL_SUBSCRIPT_SECOND^#
  }
}

extension SomeProto {
  var prop1: Int {
    #^PROTOCOL_EXT_PROPERTY_FIRST^#
  }
  var prop2: Int {
    set {} #^PROTOCOL_EXT_PROPERTY_SECOND^#
  }
  subscript(_1 index:Int) -> Int {
    #^PROTOCOL_EXT_SUBSCRIPT_FIRST^#
  }
  subscript(_2 index:Int) -> String {
    get { }
    #^PROTOCOL_EXT_SUBSCRIPT_SECOND^#
  }
}

struct SomeStruct {
  var prop1: Int {
    #^CONCRETE_PROPERTY_FIRST^#
  }
  var prop2: Int {
    get {}
    @available(*, unavailable)
    #^CONCRETE_PROPERTY_SECOND^#
  }
  subscript<T>(_1 index: T) -> Int {
    #^CONCRETE_SUBSCRIPT_FIRST^#
  }
  subscript(_2 index: Int) -> String {
    get { }
    #^CONCRETE_SUBSCRIPT_SECOND^#
  }
}

extension SomeStruct {
  var prop3: Int {
    #^CONCRETE_EXT_PROPERTY_FIRST^#
  }
  var prop4: Int {
    get {}
    #^CONCRETE_EXT_PROPERTY_SECOND^#
  }
  subscript(_3 index:Int) -> Int {
    #^CONCRETE_EXT_SUBSCRIPT_FIRST^#
  }
  subscript<U>(_4 index: Int) -> U {
    get { }
    #^CONCRETE_EXT_SUBSCRIPT_SECOND^#
  }
}

extension UNKNOWN_TYPE {
  var prop1: Int {
    #^UNKNOWN_EXT_PROPERTY_FIRST^#
  }
  var prop2: Int {
    get {}
    #^UNKNOWN_EXT_PROPERTY_SECOND^#
  }
  subscript<T>(_1 index: T) -> T where T: ANOTHER_UNKNWON_TYPE {
    #^UNKNOWN_EXT_SUBSCRIPT_FIRST^#
  }
  subscript(_2 index: Int) -> String {
    get { }
    #^UNKNOWN_EXT_SUBSCRIPT_SECOND^#
  }
}
