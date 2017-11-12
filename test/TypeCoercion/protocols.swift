// RUN: %target-typecheck-verify-swift

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

func testPrintableCoercion(_ ip1: IsPrintable1,
                           ip2: IsPrintable2,
                           inp1: IsNotPrintable1,
                           inp2: IsNotPrintable2,
                           op: OtherPrintable) {
  var p : MyPrintable = ip1 // okay
  p = ip1 // okay
  p = ip2 // okay
  p = inp1 // expected-error{{value of type 'IsNotPrintable1' does not conform to 'MyPrintable' in assignment}}
  p = inp2 // expected-error{{value of type 'IsNotPrintable2' does not conform to 'MyPrintable' in assignment}}
  p = op // expected-error{{value of type 'OtherPrintable' does not conform to 'MyPrintable' in assignment}}
  _ = p
}

func testTitledCoercion(_ ip1: IsPrintable1, book: Book, lackey: Lackey,
                        number: Number, ip2: IsPrintable2) {
  var t : Titled = ip1 // okay
  t = ip1
  t = book
  t = lackey
  t = number // expected-error{{value of type 'Number' does not conform to 'Titled' in assignment}}
  t = ip2 // expected-error{{value of type 'IsPrintable2' does not conform to 'Titled' in assignment}}
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

func testFormattedPrintableCoercion(_ ip1: IsPrintable1,
                                    ip2: IsPrintable2,
                                    fp: inout FormattedPrintable,
                                    p: inout MyPrintable,
                                    op: inout OtherPrintable,
                                    nfp1: NotFormattedPrintable1) {
  fp = ip1
  fp = ip2 // expected-error{{value of type 'IsPrintable2' does not conform to 'FormattedPrintable' in assignment}}
  fp = nfp1 // expected-error{{value of type 'NotFormattedPrintable1' does not conform to 'FormattedPrintable' in assignment}}
  p = fp
  op = fp // expected-error{{value of type 'FormattedPrintable' does not conform to 'OtherPrintable' in assignment}}
  fp = op // expected-error{{value of type 'OtherPrintable' does not conform to 'FormattedPrintable' in assignment}}
}

protocol Document : Titled, MyPrintable {
}

func testMethodsAndVars(_ fp: FormattedPrintable, f: TestFormat, doc: inout Document) {
  fp.print(f)
  fp.print()
  doc.title = "Gone with the Wind"
  doc.print()
}

func testDocumentCoercion(_ doc: inout Document, ip1: IsPrintable1, l: Lackey) {
  doc = ip1
  doc = l // expected-error{{value of type 'Lackey' does not conform to 'Document' in assignment}}
}

// Check coercion of references.
func refCoercion(_ p: inout MyPrintable) { }
var p : MyPrintable = IsPrintable1()
var fp : FormattedPrintable = IsPrintable1()
var ip1 : IsPrintable1

refCoercion(&p)
refCoercion(&fp) // expected-error{{cannot pass immutable value as inout argument: implicit conversion from 'FormattedPrintable' to 'MyPrintable' requires a temporary}}
refCoercion(&ip1) // expected-error{{cannot pass immutable value as inout argument: implicit conversion from 'IsPrintable1' to 'MyPrintable' requires a temporary}}

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

func testIntSubscripting(i_s: inout IntSubscriptable,
                         iis: IsIntSubscriptable,
                         ids: IsDoubleSubscriptable,
                         iiss: IsIntToStringSubscriptable) {
  var x = i_s[17]
  i_s[5] = 7 // expected-error{{cannot assign through subscript: subscript is get-only}}

  i_s = iis
  i_s = ids // expected-error{{value of type 'IsDoubleSubscriptable' does not conform to 'IntSubscriptable' in assignment}}
  i_s = iiss // expected-error{{value of type 'IsIntToStringSubscriptable' does not conform to 'IntSubscriptable' in assignment}}
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

func doREPLPrint(_ p: MyREPLPrintable) {
  p.myReplPrint()
}

func testREPLPrintable() {
  let i : Int = 1
  _ = i as MyREPLPrintable
  doREPLPrint(i)
  doREPLPrint(1)
  doREPLPrint("foo")
}

// Bool coercion
if true as Bool {}
