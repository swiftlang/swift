// RUN: %target-parse-verify-swift

protocol MyPrintable {
  func print()
}

protocol Titled {
  var title : String { get set }
}

struct IsPrintable1 : FormattedPrintable, Titled, Document {
  var title = ""
  func print() {}
  func print(_: TestFormat) {}
}

// Printability is below
struct IsPrintable2 { }

struct IsNotPrintable1 { }
struct IsNotPrintable2 {
  func print(_: Int) -> Int {}
}

struct Book : Titled {
  var title : String
}

struct Lackey : Titled {
  var title : String {
    get {}
    set {}
  }
}

struct Number {
  var title : Int
}

func testPrintableCoercion(ip1: IsPrintable1,
                           ip2: IsPrintable2,
                           inp1: IsNotPrintable1,
                           inp2: IsNotPrintable2,
                           op: OtherPrintable) {
  var p : MyPrintable = ip1 // okay
  p = ip1 // okay
  p = ip2 // okay
  p = inp1 // expected-error{{cannot assign a value of type 'IsNotPrintable1' to a value of type 'MyPrintable'}}
  p = inp2 // expected-error{{cannot assign a value of type 'IsNotPrintable2' to a value of type 'MyPrintable'}}
  p = op // expected-error{{cannot assign a value of type 'OtherPrintable' to a value of type 'MyPrintable'}}
  _ = p
}

func testTitledCoercion(ip1: IsPrintable1, book: Book, lackey: Lackey,
                        number: Number, ip2: IsPrintable2) {
  var t : Titled = ip1 // okay
  t = ip1
  t = book
  t = lackey
  t = number // expected-error{{cannot assign a value of type 'Number' to a value of type 'Titled'}}
  t = ip2 // expected-error{{cannot assign a value of type 'IsPrintable2' to a value of type 'Titled'}}
  _ = t
}




extension IsPrintable2 : MyPrintable {
  func print() {}
}

protocol OtherPrintable {
  func print()
}

struct TestFormat {}

protocol FormattedPrintable : MyPrintable { 
  func print(_: TestFormat)
}

struct NotFormattedPrintable1 {
  func print(_: TestFormat) { }
}

func testFormattedPrintableCoercion(ip1: IsPrintable1,
                                    ip2: IsPrintable2,
                                    inout fp: FormattedPrintable,
                                    inout p: MyPrintable,
                                    inout op: OtherPrintable,
                                    nfp1: NotFormattedPrintable1) {
  fp = ip1
  fp = ip2 // expected-error{{cannot assign a value of type 'IsPrintable2' to a value of type 'FormattedPrintable'}}
  fp = nfp1 // expected-error{{cannot assign a value of type 'NotFormattedPrintable1' to a value of type 'FormattedPrintable'}}
  p = fp
  op = fp // expected-error{{cannot assign a value of type 'FormattedPrintable' to a value of type 'OtherPrintable'}}
  fp = op // expected-error{{cannot assign a value of type 'OtherPrintable' to a value of type 'FormattedPrintable'}}
}

protocol Document : Titled, MyPrintable {
}

func testMethodsAndVars(fp: FormattedPrintable, f: TestFormat, inout doc: Document) {
  fp.print(f)
  fp.print()
  doc.title = "Gone with the Wind"
  doc.print()
}

func testDocumentCoercion(inout doc: Document, ip1: IsPrintable1, l: Lackey) {
  doc = ip1
  doc = l // expected-error{{cannot assign a value of type 'Lackey' to a value of type 'Document'}}
}

// Check coercion of references.
func refCoercion(inout p: MyPrintable) { }
var p : MyPrintable = IsPrintable1()
var fp : FormattedPrintable = IsPrintable1()
var ip1 : IsPrintable1

refCoercion(&p)
refCoercion(&fp) // expected-error{{cannot invoke 'refCoercion' with an argument list of type '(inout FormattedPrintable)'}} expected-note{{expected an argument list of type '(inout MyPrintable)'}}
refCoercion(&ip1) // expected-error{{cannot invoke 'refCoercion' with an argument list of type '(inout IsPrintable1)'}} expected-note{{expected an argument list of type '(inout MyPrintable)'}}

protocol IntSubscriptable {
  subscript(i: Int) -> Int { get }
}

struct IsIntSubscriptable : IntSubscriptable {
  subscript(i: Int) -> Int { get {} set {} }
}

struct IsDoubleSubscriptable {
  subscript(d: Double) -> Int { get {} set {} }
}

struct IsIntToStringSubscriptable {
  subscript(i: Int) -> String { get {} set {} }
}

func testIntSubscripting(inout i_s: IntSubscriptable,
                         iis: IsIntSubscriptable,
                         ids: IsDoubleSubscriptable,
                         iiss: IsIntToStringSubscriptable) {
  var x = i_s[17]
  i_s[5] = 7 // expected-error{{cannot assign to the result of this expression}}

  i_s = iis
  i_s = ids // expected-error{{cannot assign a value of type 'IsDoubleSubscriptable' to a value of type 'IntSubscriptable'}}
  i_s = iiss // expected-error{{cannot assign a value of type 'IsIntToStringSubscriptable' to a value of type 'IntSubscriptable'}}
}

protocol MyREPLPrintable {
  func myReplPrint()
}

extension Int : MyREPLPrintable {
  func myReplPrint() {}
}
extension String : MyREPLPrintable {
  func myReplPrint() {}
}

func doREPLPrint(p: MyREPLPrintable) {
  p.myReplPrint()
}

func testREPLPrintable() {
  let i : Int = 1
  _ = i as MyREPLPrintable
  doREPLPrint(i)
  doREPLPrint(1)
  doREPLPrint("foo")
}
