// RUN: %swift %s -verify

protocol Printable {
  func print()
}

protocol Titled {
  var title : String
}

// FIXME: When members of structs turn into proper variables (rather
// than tuple elements), turn these into real variables.
struct IsPrintable1 { 
  var title : String { get {} set {} }
  func print()
}

// Printability is below
struct IsPrintable2 { }

struct IsNotPrintable1 { }
struct IsNotPrintable2 { 
  func print(_ : Int) -> Int
}

struct Book {
  var title : String { get {} set {} }
}

struct Lackey {
  var title : String {
    get {}
    set {}
  }
}

struct Number {
  var title : Int { get {} set {} }
}

func testPrintableCoercion(ip1 : IsPrintable1,
                           ip2 : IsPrintable2,
                           inp1 : IsNotPrintable1,
                           inp2 : IsNotPrintable2,
                           op : OtherPrintable) {
  var p : Printable
  p = ip1 // okay
  p = ip2 // okay
  p = inp1 // expected-error{{type 'IsNotPrintable1' does not conform to protocol 'Printable'}}
  p = inp2 // expected-error{{type 'IsNotPrintable2' does not conform to protocol 'Printable'}}
  p = op
}

func testTitledCoercion(ip1 : IsPrintable1, book : Book, lackey : Lackey,
                        number : Number, ip2 : IsPrintable2) {
  var t : Titled
  t = ip1
  t = book
  t = lackey
  t = number // expected-error{{type 'Number' does not conform to protocol 'Titled'}}
  t = ip2 // expected-error{{type 'IsPrintable2' does not conform to protocol 'Titled'}}
}




extension IsPrintable2 { 
  func print()
}

protocol OtherPrintable {
  func print()
}
