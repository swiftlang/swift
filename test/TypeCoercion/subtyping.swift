// RUN: %swift %s -verify

protocol Printable {
  func print()
}

struct Format {}

protocol FormattedPrintable : Printable { 
  func print(_ : Format)
}

struct IsPrintable1 { 
  func print()
}

func accept_creates_Printable (_ : () -> Printable) {}
func accept_creates_FormattedPrintable (_ : () -> FormattedPrintable) {}

func protocolConformance(ac1 : [auto_closure] () -> Printable,
                         ac2 : [auto_closure] () -> FormattedPrintable,
                         ip1 : [auto_closure] () -> IsPrintable1) {
  var f1 : (fp : FormattedPrintable) -> Printable
  var f2 : (p : Printable) -> FormattedPrintable
  var f3 : (p : Printable) -> IsPrintable1

  f1 = f2 // okay
  f1 = f3 // okay
  f2 = f1 // expected-error{{invalid conversion from type '(fp : FormattedPrintable) -> Printable' to '(p : Printable) -> FormattedPrintable'}}

  accept_creates_Printable(ac1)
  accept_creates_Printable(ac2)
  accept_creates_Printable(ip1)
  accept_creates_FormattedPrintable(ac1) // expected-error{{invalid conversion}} expected-note{{while converting}}
  accept_creates_FormattedPrintable(ac2)
  accept_creates_FormattedPrintable(ip1) // expected-error{{invalid conversion}} expected-note{{while converting}}
}

func nonTrivialNested() {
  var f1 : (_ : () -> Printable) -> Printable
  var f2 : (_ : () -> Printable) -> FormattedPrintable
  var f3 : (_ : () -> FormattedPrintable) -> Printable

  f1 = f2 // okay
  f1 = f3 // expected-error{{invalid conversion}}
}
