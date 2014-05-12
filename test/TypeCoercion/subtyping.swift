// RUN: %swift %s -verify

protocol Printable {
  func print()
}

struct TestFormat {}

protocol FormattedPrintable : Printable { 
  func print(_: TestFormat)
}

struct IsPrintable1 : Printable {
  func print() {}
}

func accept_creates_Printable (_: () -> Printable) {}
func accept_creates_FormattedPrintable (_: () -> FormattedPrintable) {} // expected-note 2{{in call to function 'accept_creates_FormattedPrintable'}}

func fp_to_p(fp: FormattedPrintable) -> Printable { return fp; }
func p_to_fp(p: Printable) -> FormattedPrintable { }
func p_to_ip1(p: Printable) -> IsPrintable1 { }

func protocolConformance(ac1: @auto_closure () -> Printable,
                         ac2: @auto_closure () -> FormattedPrintable,
                         ip1: @auto_closure () -> IsPrintable1) {
  var f1 : (fp : FormattedPrintable) -> Printable = fp_to_p
  var f2 : (p : Printable) -> FormattedPrintable = p_to_fp
  var f3 : (p : Printable) -> IsPrintable1 = p_to_ip1

  f1 = f2 // okay
  f1 = f3 // okay
  f2 = f1 // expected-error{{'Printable' does not conform to protocol 'FormattedPrintable'}}

  accept_creates_Printable(ac1)
  accept_creates_Printable(ac2)
  accept_creates_Printable(ip1)
  accept_creates_FormattedPrintable(ac1) // expected-error{{type 'Printable' does not conform to protocol 'FormattedPrintable'}}
  accept_creates_FormattedPrintable(ac2)
  accept_creates_FormattedPrintable(ip1) // expected-error{{type 'IsPrintable1' does not conform to protocol 'FormattedPrintable'}}
}

func p_gen_to_fp(_: () -> Printable) -> FormattedPrintable {}
func fp_gen_to_p(_: () -> FormattedPrintable) -> Printable {}

func nonTrivialNested() {
  var f1 : (_ : () -> Printable) -> Printable = p_gen_to_fp
  var f2 : (_ : () -> Printable) -> FormattedPrintable = p_gen_to_fp
  var f3 : (_ : () -> FormattedPrintable) -> Printable = fp_gen_to_p

  f1 = f2 // okay
  f1 = f3 // expected-error{{'Printable' does not conform to protocol 'FormattedPrintable'}}
}
