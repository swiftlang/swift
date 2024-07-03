// RUN: %batch-code-completion

// LOCAL_STRING_TESTVALUE-NOT: name=testValue
// LOCAL_STRING_TESTVALUE:     Decl[LocalVar]/Local: testValue[#String#]; name=testValue
// LOCAL_STRING_TESTVALUE-NOT: name=testValue

// CURRNOMINAL_STRING_TESTVALUE-NOT: name=testValue
// CURRNOMINAL_STRING_TESTVALUE:     Decl[InstanceVar]/CurrNominal: testValue[#String#]; name=testValue
// CURRNOMINAL_STRING_TESTVALUE-NOT: name=testValue

// CURRNOMINAL_STRING_TESTVALUE_STATIC-NOT: name=testValue
// CURRNOMINAL_STRING_TESTVALUE_STATIC:     Decl[StaticVar]/CurrNominal: testValue[#String#]; name=testValue
// CURRNOMINAL_STRING_TESTVALUE_STATIC-NOT: name=testValue

// SUPER_STRING_TESTVALUE-NOT: name=testValue
// SUPER_STRING_TESTVALUE:     Decl[InstanceVar]/Super: testValue[#String#]; name=testValue
// SUPER_STRING_TESTVALUE-NOT: name=testValue

// OUTNOMINAL_STRING_TESTVALUE_STATIC-NOT: name=testValue
// OUTNOMINAL_STRING_TESTVALUE_STATIC:     Decl[StaticVar]/OutNominal: testValue[#String#]; name=testValue
// OUTNOMINAL_STRING_TESTVALUE_STATIC-NOT: name=testValue
  
// GENERICPARAM_TESTVALUE-NOT: name=testValue
// GENERICPARAM_TESTVALUE:  Decl[GenericTypeParam]/Local: testValue[#testValue#]; name=testValue
// GENERICPARAM_TESTVALUE-NOT: name=testValue

func test_Param_Local(testValue: Int) {
  var testValue: String = ""
  #^Param_Local?check=LOCAL_STRING_TESTVALUE^#
}

func test_Local_BindingIf(optStr: String?) {
  var testValue: Int = 12
  if let testValue = optStr {
    #^Local_BindingIf?check=LOCAL_STRING_TESTVALUE^#
  }
}

func test_Local_BindingGuard(optStr: String?) {
  var testValue: Int = 12
  guard var testValue = optStr else {
    return
  }
  #^Local_BindingGuard?check=LOCAL_STRING_TESTVALUE^#
}

func test_Local_LocalDo() {
  var testValue: Int = 12
  do {
    var testValue: String = ""
    #^Local_LocalDo?check=LOCAL_STRING_TESTVALUE^#
  }
}

func test_Local_LocalFunc() {
  var testValue: Int = 12
  func inner() {
    var testValue: String = ""
    #^Local_LocalFunc?check=LOCAL_STRING_TESTVALUE^#
  }
}

func test_Local_Member() {
  var testValue: Int = 12
  struct Inner {
    var testValue: String = ""
    func test() {
      #^Local_Member?check=CURRNOMINAL_STRING_TESTVALUE^#
    }
  }
}

protocol ProtoWithStringTestValueReq {
  var testValue: String { get }
}
func test_Local_InheritedMemberProtoReq() {
  var testValue: Int = 12
  struct Inner: ProtoWithStringTestValueReq {
    func test() {
      #^Local_InheritedMemberProtoReq?check=SUPER_STRING_TESTVALUE^#
    }
  }
}

protocol ProtoWithStringTestValueExt {}
extension ProtoWithStringTestValueExt {
  var testValue: String { "" }
}
func test_Local_InheritedMemberProtoExt() {
  var testValue: Int = 12
  struct Inner: ProtoWithStringTestValueExt {
    func test() {
      #^Local_InheritedMemberProtoExt?check=SUPER_STRING_TESTVALUE^#
    }
  }
}

class ClassWithStringTestValue {
  var testValue: String { "" }
}
func test_Local_InheritedMemberSuper() {
  var testValue: Int = 12
  class Inner: ClassWithStringTestValue {
    func test() {
      #^Local_InheritedMemberSuper?check=SUPER_STRING_TESTVALUE^#
    }
  }
}

struct test_Member_Local {
  var testValue: Int = 12
  func test() {
    var testValue: String = ""
    #^Member_Local?check=LOCAL_STRING_TESTVALUE^#
  }
}

struct test_OutNominal_Member {
  static var testValue: Int = 12
  struct Inner {
    static var testValue: String = ""
    static func test() {
      #^OutNominal_Member?check=CURRNOMINAL_STRING_TESTVALUE_STATIC^#
    }
  }
}

struct test_OutNominal_Local {
  static var testValue: Int = 12
  struct Inner {
    static func test() {
      var testValue: String = ""
      #^OutNominal_Local?check=LOCAL_STRING_TESTVALUE^#
    }
  }
}

func test_Local_OutNominal() {
  var testValue: Int = 12
  struct Outer {
    static var testValue: String = ""
    struct Inner {
      static func test() {
        #^Local_OutNominal?check=OUTNOMINAL_STRING_TESTVALUE_STATIC^#
      }
    }
  }
}

var globalTest: Int = 1
func test_Global_Local() {
  var globalTest: String = ""
  #^Global_Local^#
// FIXME: currently global variables are suggested despite they are shadowed.
// Ideally they should be suggested with the qualification (i.e. 'ModuleName.globalTest')
// Global_Local-DAG: Decl[LocalVar]/Local:               globalTest[#String#]; name=globalTest
// Global_Local-DAG: Decl[GlobalVar]/CurrModule:         globalTest[#Int#]; name=globalTest
}

func test_GenericParam_Local<testValue>(_: testValue) {
  var testValue: String = ""
  #^GenericParam_Local?check=LOCAL_STRING_TESTVALUE^#
}

func test_GenericParam_Param<testValue>(testValue: String, _: testValue) {
  #^GenericParam_Param?check=LOCAL_STRING_TESTVALUE^#
}

func test_GenericParam_LocalClosure<testValue>(_: testValue) {
  _ =  {
    var testValue: String = ""
    #^GenericParam_LocalClosure?check=LOCAL_STRING_TESTVALUE^#
  }
}

func test_Local_GenericParamType() {
  var testValue: String = ""
  struct S<testValue> {
    var test = #^Local_GenericParamType?check=GENERICPARAM_TESTVALUE^#
  }
}

func test_Local_GenericParamType() {
  var testValue: String = ""
  func test<testValue>(_: testValue) {
    #^Local_GenericParam?check=GENERICPARAM_TESTVALUE^#
  }
}

func test_LocalFunc_LocalVar() {
  func testValue(arg: Int) {}
  var testValue: String = ""
  #^LocalFunc_LocalVar^#
// LocalFunc_LocalVar-NOT: name=testValue
// LocalFunc_LocalVar-DAG: Decl[LocalVar]/Local:               testValue[#String#]; name=testValue
// LocalFunc_LocalVar-NOT: name=testValue
}

func test_LocalFunc_LocalVarDo() {
  func testValue(arg: Int) {}
  do {
    var testValue: String = ""
    #^LocalFunc_LocalVarDo?check=LOCAL_STRING_TESTVALUE^#
  }
}

func test_LocalVar_LocalFuncDo() {
  var testValue: String = ""
  do {
    func testValue(arg: Int) {}
    #^LocalVar_LocalFuncDo^#
// FIXME: 'var testValue' is actually shadowed and not usable. 'Decl[LocalVar]/Local: testValue[#String#]' should not be suggested.
// LocalVar_LocalFuncDo-NOT: name=testValue
// LocalVar_LocalFuncDo-DAG: Decl[FreeFunction]/Local:           testValue({#arg: Int#})[#Void#]; name=testValue(arg:)
// LocalVar_LocalFuncDo-DAG: Decl[LocalVar]/Local:               testValue[#String#]; name=testValue
// LocalVar_LocalFuncDo-NOT: name=testValue
  }
}

struct test_Member_Member {
  var testValue: String = ""
  func testValue(arg: String) {}

  func test() {
    #^Member_Member^#
// Member_Member-NOT: name=testValue
// Member_Member-DAG: Decl[InstanceVar]/CurrNominal:      testValue[#String#]; name=testValue
// Member_Member-DAG: Decl[InstanceMethod]/CurrNominal:   testValue({#arg: String#})[#Void#]; name=testValue(arg:)
// Member_Member-NOT: name=testValue
  }
}

protocol ProtoWithIntTestValueReq {
  var testValue: Int { get }
}
struct test_InheritedMemberProtoReq_Member: ProtoWithIntTestValueReq {
  var testValue: String = ""

  func test() {
    #^InheritedMemberProtoReq_Member?check=CURRNOMINAL_STRING_TESTVALUE^#
  }
}

protocol ProtoWithIntTestValueExt {}
extension ProtoWithIntTestValueExt {
  var testValue: Int { 1 }
}
struct test_InheritedMemberProtoExt_Member: ProtoWithIntTestValueExt {
  var testValue: String = ""

  func test() {
    #^InheritedMemberProtoExt_Member^#
// InheritedMemberProtoExt_Member-NOT: name=testValue
// InheritedMemberProtoExt_Member-DAG: Decl[InstanceVar]/CurrNominal:      testValue[#String#]; name=testValue
// InheritedMemberProtoExt_Member-DAG: Decl[InstanceVar]/Super:            testValue[#Int#]; name=testValue
// InheritedMemberProtoExt_Member-NOT: name=testValue
  }
}

protocol ClassWithIntTestValue {
  var testValue: Int { 1 }
}
struct test_InheritedMemberSuper_Member: ClassWithIntTestValue {
  var testValue: String = ""

  func test() {
    #^InheritedMemberSuper?check=CURRNOMINAL_STRING_TESTVALUE^#
  }
}

func test_LocalTy_OutNominalTy() {
  struct testValue {} // Local
  struct Outer {
    class testValue {} // OutNominal
    struct Inner {
      func test() {
        #^LocalTy_OutNominalTy^#
// LocalTy_OutNominalTy-NOT: name=testValue
// LocalTy_OutNominalTy-DAG: Decl[Class]/OutNominal: testValue[#Outer.testValue#]; name=testValue
// LocalTy_OutNominalTy-NOT: name=testValue
      }
    }
  }
}
