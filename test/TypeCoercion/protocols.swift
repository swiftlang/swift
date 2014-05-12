// RUN: %swift %s -verify

protocol Printable {
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
  var p : Printable = ip1 // okay
  p = ip1 // okay
  p = ip2 // okay
  p = inp1 // expected-error{{'IsNotPrintable1' does not conform to protocol 'Printable'}}
  p = inp2 // expected-error{{'IsNotPrintable2' does not conform to protocol 'Printable'}}
  p = op // expected-error{{type 'OtherPrintable' does not conform to protocol 'Printable'}}
}

func testTitledCoercion(ip1: IsPrintable1, book: Book, lackey: Lackey,
                        number: Number, ip2: IsPrintable2) {
  var t : Titled = ip1 // okay
  t = ip1
  t = book
  t = lackey
  t = number // expected-error{{'Number' does not conform to protocol 'Titled'}}
  t = ip2 // expected-error{{'IsPrintable2' does not conform to protocol 'Titled'}}
}




extension IsPrintable2 : Printable {
  func print() {}
}

protocol OtherPrintable {
  func print()
}

struct TestFormat {}

protocol FormattedPrintable : Printable { 
  func print(_: TestFormat)
}

struct NotFormattedPrintable1 {
  func print(_: TestFormat) { }
}

func testFormattedPrintableCoercion(ip1: IsPrintable1,
                                    ip2: IsPrintable2,
                                    inout fp: FormattedPrintable,
                                    inout p: Printable,
                                    inout op: OtherPrintable,
                                    nfp1: NotFormattedPrintable1) {
  fp = ip1
  fp = ip2 // expected-error{{'IsPrintable2' does not conform to protocol 'FormattedPrintable'}}
  fp = nfp1 // expected-error{{'NotFormattedPrintable1' does not conform to protocol 'FormattedPrintable'}}
  p = fp
  op = fp // expected-error{{type 'FormattedPrintable' does not conform to protocol 'OtherPrintable'}}
  fp = op // expected-error{{'OtherPrintable' does not conform to protocol 'FormattedPrintable'}}
}

protocol Document : Titled, Printable {
}

func testMethodsAndVars(fp: FormattedPrintable, f: TestFormat, inout doc: Document) {
  fp.print(f)
  fp.print()
  doc.title = "Gone with the Wind"
  doc.print()
}

func testDocumentCoercion(inout doc: Document, ip1: IsPrintable1, l: Lackey) {
  doc = ip1
  doc = l // expected-error{{'Lackey' does not conform to protocol 'Document'}}
}

// Check coercion of references.
func refCoercion(inout p: Printable) { } // expected-note 2{{in initialization of parameter 'p'}}
var p : Printable = IsPrintable1()
var fp : FormattedPrintable = IsPrintable1()
var ip1 : IsPrintable1

refCoercion(&p)
refCoercion(&fp) // expected-error{{'FormattedPrintable' is not identical to 'Printable'}}
refCoercion(&ip1) // expected-error{{'IsPrintable1' is not identical to 'Printable'}}

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
  i_s = ids // expected-error{{'IsDoubleSubscriptable' does not conform to protocol 'IntSubscriptable'}}
  i_s = iiss // expected-error{{'IsIntToStringSubscriptable' does not conform to protocol 'IntSubscriptable'}}
}

protocol REPLPrintable {
  func replPrint()
}

extension Int : REPLPrintable { }
extension String : REPLPrintable { }

func doREPLPrint(p: REPLPrintable) {
  p.replPrint()
}

func testREPLPrintable() {
  var i : Int
  var rp : REPLPrintable = i
  doREPLPrint(i)
  doREPLPrint(1)
  doREPLPrint("foo")
}

// rdar://16041990
class CoercionTarget {}
struct CoercesToClass {
  func __conversion() -> CoercionTarget { return CoercionTarget() }
}
func testNoCoercionDowncast(v : AnyObject) {
  let maybeClass = v as CoercionTarget
  let maybeStruct = v as CoercesToClass // expected-error {{cannot convert the expression's type '$T1' to type '$T2'}}
}
