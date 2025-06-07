// RUN: %batch-code-completion

var globalAsyncVar: Int {
  get async {
    42
  }
}

var globalAsyncThrowingVar: Int {
  get async throws {
    42
  }
}

struct Foo {
  var asyncProperty: Int {
    get async {
      42
    }
  }

  var asyncThrowingProperty: Int {
    get async throws {
      42
    }
  }

  func asyncFunc() async -> Int { 42 }
  func asyncThrowingFunc() async throws -> Int { 42 }

  func testMember(f: Foo) async throws {
    f.#^MEMBER^#
  }

  func testGlobal() async throws {
    #^GLOBAL^#
  }
}

// MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      asyncProperty[#Int#][' async'];
// MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      asyncThrowingProperty[#Int#][' async'][' throws'];
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal:   asyncFunc()[' async'][#Int#];
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal:   asyncThrowingFunc()[' async'][' throws'][#Int#];

// GLOBAL-DAG: Decl[GlobalVar]/CurrModule:         globalAsyncVar[#Int#][' async'];
// GLOBAL-DAG: Decl[GlobalVar]/CurrModule:         globalAsyncThrowingVar[#Int#][' async'][' throws']
