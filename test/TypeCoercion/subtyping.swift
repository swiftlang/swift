// RUN: %target-parse-verify-swift

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

func accept_creates_Printable (@noescape _: () -> Printable) {}
func accept_creates_FormattedPrintable (@noescape _: () -> FormattedPrintable) {}

func fp_to_p(fp: FormattedPrintable) -> Printable { return fp; }
func p_to_fp(p: Printable) -> FormattedPrintable { }
func p_to_ip1(p: Printable) -> IsPrintable1 { }

func protocolConformance(@autoclosure ac1: () -> Printable,
                         @autoclosure ac2: () -> FormattedPrintable,
                         @autoclosure ip1: () -> IsPrintable1) {
  var f1 : (fp : FormattedPrintable) -> Printable = fp_to_p
  var f2 : (p : Printable) -> FormattedPrintable = p_to_fp
  var f3 : (p : Printable) -> IsPrintable1 = p_to_ip1

  f1 = f2 // okay
  f1 = f3 // okay
  f2 = f1 // expected-error{{cannot assign a value of type '(fp: FormattedPrintable) -> Printable' to a value of type '(p: Printable) -> FormattedPrintable'}}

  accept_creates_Printable(ac1)
  accept_creates_Printable(ac2)
  accept_creates_Printable(ip1)
  accept_creates_FormattedPrintable(ac1) // expected-error{{cannot invoke 'accept_creates_FormattedPrintable' with an argument list of type '(@autoclosure () -> Printable)'}} expected-note{{expected an argument list of type '(@noescape () -> FormattedPrintable)'}}
  accept_creates_FormattedPrintable(ac2)
  accept_creates_FormattedPrintable(ip1) // expected-error{{cannot invoke 'accept_creates_FormattedPrintable' with an argument list of type '(@autoclosure () -> IsPrintable1)'}} expected-note{{expected an argument list of type '(@noescape () -> FormattedPrintable)'}}
}

func p_gen_to_fp(_: () -> Printable) -> FormattedPrintable {}
func fp_gen_to_p(_: () -> FormattedPrintable) -> Printable {}

func nonTrivialNested() {
  var f1 : (_ : () -> Printable) -> Printable = p_gen_to_fp
  var f2 : (_ : () -> Printable) -> FormattedPrintable = p_gen_to_fp
  var f3 : (_ : () -> FormattedPrintable) -> Printable = fp_gen_to_p

  f1 = f2 // okay
  f1 = f3 // expected-error{{annot assign a value of type '(() -> FormattedPrintable) -> Printable' to a value of type '(() -> Printable) -> Printable'}}
}
