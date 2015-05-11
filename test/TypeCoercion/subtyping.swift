// RUN: %target-parse-verify-swift

protocol CustomStringConvertible {
  func print()
}

struct TestFormat {}

protocol FormattedPrintable : CustomStringConvertible { 
  func print(_: TestFormat)
}

struct IsPrintable1 : CustomStringConvertible {
  func print() {}
}

func accept_creates_Printable (@noescape _: () -> CustomStringConvertible) {}
func accept_creates_FormattedPrintable (@noescape _: () -> FormattedPrintable) {}

func fp_to_p(fp: FormattedPrintable) -> CustomStringConvertible { return fp; }
func p_to_fp(p: CustomStringConvertible) -> FormattedPrintable { }
func p_to_ip1(p: CustomStringConvertible) -> IsPrintable1 { }

func protocolConformance(@autoclosure ac1: () -> CustomStringConvertible,
                         @autoclosure ac2: () -> FormattedPrintable,
                         @autoclosure ip1: () -> IsPrintable1) {
  var f1 : (fp : FormattedPrintable) -> CustomStringConvertible = fp_to_p
  var f2 : (p : CustomStringConvertible) -> FormattedPrintable = p_to_fp
  var f3 : (p : CustomStringConvertible) -> IsPrintable1 = p_to_ip1

  // FIXME: closures make ABI conversions explicit. rdar://problem/19517003
  f1 = { f2(p: $0) } // okay
  f1 = { f3(p: $0) } // okay
  f2 = f1 // expected-error{{cannot assign a value of type '(fp: FormattedPrintable) -> CustomStringConvertible' to a value of type '(p: CustomStringConvertible) -> FormattedPrintable'}}

  accept_creates_Printable(ac1)
  accept_creates_Printable({ ac2($0) })
  accept_creates_Printable({ ip1($0) })
  accept_creates_FormattedPrintable(ac1) // expected-error{{cannot invoke 'accept_creates_FormattedPrintable' with an argument list of type '(@autoclosure () -> CustomStringConvertible)'}} expected-note{{expected an argument list of type '(@noescape () -> FormattedPrintable)'}}
  accept_creates_FormattedPrintable(ac2)
  accept_creates_FormattedPrintable(ip1) // expected-error{{cannot invoke 'accept_creates_FormattedPrintable' with an argument list of type '(@autoclosure () -> IsPrintable1)'}} expected-note{{expected an argument list of type '(@noescape () -> FormattedPrintable)'}}
}

func p_gen_to_fp(_: () -> CustomStringConvertible) -> FormattedPrintable {}
func fp_gen_to_p(_: () -> FormattedPrintable) -> CustomStringConvertible {}

func nonTrivialNested() {
  // FIXME: closures make ABI conversions explicit. rdar://problem/19517003
  var f1 : (_ : () -> CustomStringConvertible) -> CustomStringConvertible = { p_gen_to_fp($0) }
  let f2 : (_ : () -> CustomStringConvertible) -> FormattedPrintable = p_gen_to_fp
  let f3 : (_ : () -> FormattedPrintable) -> CustomStringConvertible = fp_gen_to_p

  f1 = { f2($0) } // okay
  f1 = f3 // expected-error{{annot assign a value of type '(() -> FormattedPrintable) -> CustomStringConvertible' to a value of type '(() -> CustomStringConvertible) -> CustomStringConvertible'}}
  let _ = f1
}
