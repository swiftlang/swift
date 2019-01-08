// RUN: %target-typecheck-verify-swift

import AppKit

let arrayIllegal = [,] // expected-error{{expected expression in container literal}}
let array0 = [,,1] // expected-error{{expected expression in container literal}} expected-error{{expected expression in container literal}}
let array1 = [,1] // expected-error{{expected expression in container literal}}
let array2 = [1]
let array3 = [1,]
let array4 = [1,,] // expected-error{{expected expression in container literal}}


let dictionary0 = [,,1:2] // expected-error{{expected expression in container literal}} expected-error{{expected expression in container literal}}
let dictionary1 = [,1:2] // expected-error{{expected expression in container literal}}
let dictionary2 = [1:2]
let dictionary3 = [1:2,]
let dictionary4 = [1:2,,] // expected-error{{expected key expression in dictionary literal}}



let c0: NSColor = #colorLiteral(,,red: 1.0, green: 1.0, blue: 1.0, alpha: 1.0) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let c1: NSColor = #colorLiteral(,red: 1.0, green: 1.0, blue: 1.0, alpha: 1.0) // expected-error{{unexpected ',' separator}}
let c2: NSColor = #colorLiteral(red: 1.0, green: 1.0, blue: 1.0, alpha: 1.0)
let c3: NSColor = #colorLiteral(red: 1.0, green: 1.0, blue: 1.0, alpha: 1.0,)
let c4: NSColor = #colorLiteral(red: 1.0, green: 1.0, blue: 1.0, alpha: 1.0,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let f0: URL = #fileLiteral(,,resourceName: "unexpected ',' separator") // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let f1: URL = #fileLiteral(,resourceName: "unexpected ',' separator") // expected-error{{unexpected ',' separator}}
let f2: URL = #fileLiteral(resourceName: "unexpected ',' separator")
let f3: URL = #fileLiteral(resourceName: "unexpected ',' separator",)
let f4: URL = #fileLiteral(resourceName: "unexpected ',' separator",,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let i0: NSImage = #imageLiteral(,,resourceName: "fancy") // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let i1: NSImage = #imageLiteral(,resourceName: "fancy") // expected-error{{unexpected ',' separator}}
let i2: NSImage = #imageLiteral(resourceName: "fancy")
let i3: NSImage = #imageLiteral(resourceName: "fancy",)
let i4: NSImage = #imageLiteral(resourceName: "fancy",,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


func noargs() {}

noargs()
noargs(,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
noargs(,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


func fn_onearg__firstarg_nolabel(_ int: Int) {}

fn_onearg__firstarg_nolabel(,,-2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} 
fn_onearg__firstarg_nolabel(,-1) // expected-error{{unexpected ',' separator}}
fn_onearg__firstarg_nolabel(0)
fn_onearg__firstarg_nolabel(1,)
fn_onearg__firstarg_nolabel(2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


func fn_onearg__firstarg_label(int: Int) {}

fn_onearg__firstarg_label(,,int: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
fn_onearg__firstarg_label(,int: -1) // expected-error{{unexpected ',' separator}}
fn_onearg__firstarg_label(int: 0)
fn_onearg__firstarg_label(int: 1,)
fn_onearg__firstarg_label(int: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


func fn_twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {}

fn_twoarg__firstarg_nolabel__secondarg_nolabel(,,-2, -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_nolabel__secondarg_nolabel(,-1, -1) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_nolabel__secondarg_nolabel(0, 0)
fn_twoarg__firstarg_nolabel__secondarg_nolabel(0,, 0) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_nolabel__secondarg_nolabel(1, 1,)
fn_twoarg__firstarg_nolabel__secondarg_nolabel(1,, 1,) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_nolabel__secondarg_nolabel(2, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} 
fn_twoarg__firstarg_nolabel__secondarg_nolabel(2,, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


func fn_twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {}

fn_twoarg__firstarg_label__secondarg_nolabel(,,first: -2, -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_label__secondarg_nolabel(,first: -1, -1) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_label__secondarg_nolabel(first: 0, 0)
fn_twoarg__firstarg_label__secondarg_nolabel(first: 0,, 0) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_label__secondarg_nolabel(first: 1, 1,)
fn_twoarg__firstarg_label__secondarg_nolabel(first: 1,, 1,) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_label__secondarg_nolabel(first: 2, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
fn_twoarg__firstarg_label__secondarg_nolabel(first: 2,, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


func fn_twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {}

fn_twoarg__firstarg_nolabel__secondarg_label(,,-2, second: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_nolabel__secondarg_label(,-1, second: -1) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_nolabel__secondarg_label(0, second: 0)
fn_twoarg__firstarg_nolabel__secondarg_label(0,, second: 0) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_nolabel__secondarg_label(1, second: 1,)
fn_twoarg__firstarg_nolabel__secondarg_label(1,, second: 1,) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_nolabel__secondarg_label(2, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
fn_twoarg__firstarg_nolabel__secondarg_label(2,, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


func fn_twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {}

fn_twoarg__firstarg_label__secondarg_label(,,first: -2, second: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_label__secondarg_label(,first: -1, second: -1) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_label__secondarg_label(first: 0, second: 0)
fn_twoarg__firstarg_label__secondarg_label(first: 0,, second: 0) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_label__secondarg_label(first: 1, second: 1,)
fn_twoarg__firstarg_label__secondarg_label(first: 1,, second: 1,) // expected-error{{unexpected ',' separator}}
fn_twoarg__firstarg_label__secondarg_label(first: 2, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
fn_twoarg__firstarg_label__secondarg_label(first: 2,, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S1 {
    func noargs() {}
}

let s1 = S1()

s1.noargs() 
s1.noargs(,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s1.noargs(,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S2 {
    func onearg__firstarg_nolabel(_ int: Int) {}
}

let s2 = S2()

s2.onearg__firstarg_nolabel(,,-2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s2.onearg__firstarg_nolabel(,-1) // expected-error{{unexpected ',' separator}}
s2.onearg__firstarg_nolabel(0)
s2.onearg__firstarg_nolabel(1,)
s2.onearg__firstarg_nolabel(2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S3 {
    func onearg__firstarg_label(int: Int) {}
}

let s3 = S3()

s3.onearg__firstarg_label(,,int: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s3.onearg__firstarg_label(,int: -1) // expected-error{{unexpected ',' separator}}
s3.onearg__firstarg_label(int: 0)
s3.onearg__firstarg_label(int: 1,)
s3.onearg__firstarg_label(int: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S4 {
    func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {}
}

let s4 = S4()

s4.twoarg__firstarg_nolabel__secondarg_nolabel(,,-2, -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s4.twoarg__firstarg_nolabel__secondarg_nolabel(,-1, -1) // expected-error{{unexpected ',' separator}}
s4.twoarg__firstarg_nolabel__secondarg_nolabel(0, 0)
s4.twoarg__firstarg_nolabel__secondarg_nolabel(0,, 0) // expected-error{{unexpected ',' separator}}
s4.twoarg__firstarg_nolabel__secondarg_nolabel(1, 1,)
s4.twoarg__firstarg_nolabel__secondarg_nolabel(1,, 1,) // expected-error{{unexpected ',' separator}}
s4.twoarg__firstarg_nolabel__secondarg_nolabel(2, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s4.twoarg__firstarg_nolabel__secondarg_nolabel(2,, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S5 {
    func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {}
}

let s5 = S5()

s5.twoarg__firstarg_label__secondarg_nolabel(,,first: -2, -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s5.twoarg__firstarg_label__secondarg_nolabel(,first: -1, -1) // expected-error{{unexpected ',' separator}}
s5.twoarg__firstarg_label__secondarg_nolabel(first: 0, 0)
s5.twoarg__firstarg_label__secondarg_nolabel(first: 0,, 0) // expected-error{{unexpected ',' separator}}
s5.twoarg__firstarg_label__secondarg_nolabel(first: 1, 1,)
s5.twoarg__firstarg_label__secondarg_nolabel(first: 1,, 1,) // expected-error{{unexpected ',' separator}}
s5.twoarg__firstarg_label__secondarg_nolabel(first: 2, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s5.twoarg__firstarg_label__secondarg_nolabel(first: 2,, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S6 {
    func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {}
}

let s6 = S6()

s6.twoarg__firstarg_nolabel__secondarg_label(,,-2, second: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s6.twoarg__firstarg_nolabel__secondarg_label(,-1, second: -1) // expected-error{{unexpected ',' separator}}
s6.twoarg__firstarg_nolabel__secondarg_label(0, second: 0)
s6.twoarg__firstarg_nolabel__secondarg_label(0,, second: 0) // expected-error{{unexpected ',' separator}}
s6.twoarg__firstarg_nolabel__secondarg_label(1, second: 1,)
s6.twoarg__firstarg_nolabel__secondarg_label(1,, second: 1,) // expected-error{{unexpected ',' separator}}
s6.twoarg__firstarg_nolabel__secondarg_label(2, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s6.twoarg__firstarg_nolabel__secondarg_label(2,, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} 


struct S7 {
    func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {}
}

let s7 = S7()

s7.twoarg__firstarg_label__secondarg_label(,,first: -2, second: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s7.twoarg__firstarg_label__secondarg_label(,first: -1, second: -1) // expected-error{{unexpected ',' separator}}
s7.twoarg__firstarg_label__secondarg_label(first: 0, second: 0)
s7.twoarg__firstarg_label__secondarg_label(first: 0,, second: 0) // expected-error{{unexpected ',' separator}}
s7.twoarg__firstarg_label__secondarg_label(first: 1, second: 1,)
s7.twoarg__firstarg_label__secondarg_label(first: 1,, second: 1,) // expected-error{{unexpected ',' separator}}
s7.twoarg__firstarg_label__secondarg_label(first: 2, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s7.twoarg__firstarg_label__secondarg_label(first: 2,, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} 


struct S8 {
    subscript() -> Int { get { return 0 } set {} }
}

var s8 = S8()

let subs00 = s8[]
let subs01 = s8[,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let subs02 = s8[,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

s8[] = 1
s8[,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s8[,,] = 3 // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S9 {
    subscript(nolabel: Int) -> Int { get { return 0 } set {} }
}

var s9 = S9()

let subs10 = s9[,,0]  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let subs11 = s9[,0] // expected-error{{unexpected ',' separator}}
let subs12 = s9[0]
let subs13 = s9[0,]
let subs14 = s9[0,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

s9[,,0] = -2  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s9[,0] = -1 // expected-error{{unexpected ',' separator}}
s9[0] = 0
s9[0,] = 1
s9[0,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S10 {
    subscript(label label: Int) -> Int { get { return 0 } set {} }
}

var s10 = S10()

let subs20 = s10[,,label: 0]  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let subs21 = s10[,label: 0] // expected-error{{unexpected ',' separator}}
let subs22 = s10[label: 0]
let subs23 = s10[label: 0,]
let subs24 = s10[label: 0,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

s10[,,label: 0] = -2  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s10[,label: 0] = -1 // expected-error{{unexpected ',' separator}}
s10[label: 0] = 0
s10[label: 0,] = 1
s10[label: 0,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S11 {
    subscript(first: Int, second: Int) -> Int { get { return 0 } set {} }
}

var s11 = S11()

let subs30 = s11[,,0, 0]  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let subs31 = s11[,0, 0] // expected-error{{unexpected ',' separator}}
let subs32 = s11[0, 0]
let subs33 = s11[0,, 0] // expected-error{{unexpected ',' separator}}
let subs34 = s11[1, 1,]
let subs35 = s11[1,, 1,] // expected-error{{unexpected ',' separator}}
let subs36 = s11[2, 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let subs37 = s11[2,, 2,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

s11[,,0, 0] = -2  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s11[,0, 0] = -1 // expected-error{{unexpected ',' separator}}
s11[0, 0] = 0
s11[0,, 0] = 0 // expected-error{{unexpected ',' separator}}
s11[1, 1,] = 1
s11[1,, 1,] = 1 // expected-error{{unexpected ',' separator}}
s11[2, 2,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s11[2,, 2,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S12 {
    subscript(first first: Int, second: Int) -> Int { get { return 0 } set {} }
}

var s12 = S12()

let subs40 = s12[,,first: 0, 0]  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let subs41 = s12[,first: 0, 0] // expected-error{{unexpected ',' separator}}
let subs42 = s12[first: 0, 0]
let subs43 = s12[first: 0,, 0] // expected-error{{unexpected ',' separator}}
let subs44 = s12[first: 1, 1,]
let subs45 = s12[first: 1,, 1,] // expected-error{{unexpected ',' separator}}
let subs46 = s12[first: 2, 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let subs47 = s12[first: 2,, 2,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

s12[,,first: 0, 0] = -2  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s12[,first: 0, 0] = -1 // expected-error{{unexpected ',' separator}}
s12[first: 0, 0] = 0
s12[first: 0,, 0] = 0 // expected-error{{unexpected ',' separator}}
s12[first: 1, 1,] = 1
s12[first: 1,, 1,] = 1 // expected-error{{unexpected ',' separator}}
s12[first: 2, 2,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s12[first: 2,, 2,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S13 {
    subscript(first: Int, second second: Int) -> Int { get { return 0 } set {} }
}

var s13 = S13()

let subs50 = s13[,,0, second: 0]  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let subs51 = s13[,0, second: 0] // expected-error{{unexpected ',' separator}}
let subs52 = s13[0, second: 0]
let subs53 = s13[0,, second: 0] // expected-error{{unexpected ',' separator}}
let subs54 = s13[1, second: 1,]
let subs55 = s13[1,, second: 1,] // expected-error{{unexpected ',' separator}}
let subs56 = s13[2, second: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let subs57 = s13[2,, second: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

s13[,,0, second: 0] = -2  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s13[,0, second: 0] = -1 // expected-error{{unexpected ',' separator}}
s13[0, second: 0] = 0
s13[0,, second: 0] = 0 // expected-error{{unexpected ',' separator}}
s13[1, second: 1,] = 1
s13[1,, second: 1,] = 1 // expected-error{{unexpected ',' separator}}
s13[2, second: 2,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s13[2,, second: 2,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct S14 {
    subscript(first first: Int, second second: Int) -> Int { get { return 0 } set {} }
}

var s14 = S14()

let subs60 = s14[,,first: 0, second: 0]  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let subs61 = s14[,first: 0, second: 0] // expected-error{{unexpected ',' separator}}
let subs62 = s14[first: 0, second: 0]
let subs63 = s14[first: 0,, second: 0] // expected-error{{unexpected ',' separator}}
let subs64 = s14[first: 1, second: 1,]
let subs65 = s14[first: 1,, second: 1,] // expected-error{{unexpected ',' separator}}
let subs66 = s14[first: 2, second: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let subs67 = s14[first: 2,, second: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

s14[,,first: 0, second: 0] = -2  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
s14[,first: 0, second: 0] = -1 // expected-error{{unexpected ',' separator}}
s14[first: 0, second: 0] = 0
s14[first: 0,, second: 0] = 0 // expected-error{{unexpected ',' separator}}
s14[first: 1, second: 1,] = 1
s14[first: 1,, second: 1,] = 1 // expected-error{{unexpected ',' separator}}
s14[first: 2, second: 2,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
s14[first: 2,, second: 2,,] = 2 // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


class B1 {
    func noargs() {}
}

class D11 : B1 {
    override func noargs() {
        super.noargs()
    }
}

class D12 : B1 {
    override func noargs() {
        super.noargs(,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class D13 : B1 {
    override func noargs() {
        super.noargs(,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class B2 {
    func onearg__firstarg_nolabel(_ int: Int) {}
}

class B21 : B2 {
    override func onearg__firstarg_nolabel(_ int: Int) {
        super.onearg__firstarg_nolabel(,,-2)  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class B22 : B2 {
    override func onearg__firstarg_nolabel(_ int: Int) {
        super.onearg__firstarg_nolabel(,-1)  // expected-error{{unexpected ',' separator}}
    }
}

class B23 : B2 {
    override func onearg__firstarg_nolabel(_ int: Int) {
        super.onearg__firstarg_nolabel(0)
    }
}

class B24 : B2 {
    override func onearg__firstarg_nolabel(_ int: Int) {
        super.onearg__firstarg_nolabel(1,)
    }
}

class B25 : B2 {
    override func onearg__firstarg_nolabel(_ int: Int) {
        super.onearg__firstarg_nolabel(1,,)  // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class B3 {
    func onearg__firstarg_label(int: Int) {}
}

class B31 : B3 {
    override func onearg__firstarg_label(int: Int) {
        super.onearg__firstarg_label(,,int: -2)  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class B32 : B3 {
    override func onearg__firstarg_label(int: Int) {
        super.onearg__firstarg_label(,int: -1)  // expected-error{{unexpected ',' separator}}
    }
}

class B33 : B3 {
    override func onearg__firstarg_label(int: Int) {
        super.onearg__firstarg_label(int: 0)
    }
}

class B34 : B3 {
    override func onearg__firstarg_label(int: Int) {
        super.onearg__firstarg_label(int: 1,)
    }
}

class B35 : B3 {
    override func onearg__firstarg_label(int: Int) {
        super.onearg__firstarg_label(int: 1,,)  // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class B4 {
    func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {}
}

class B41 : B4 {
    override func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_nolabel(,,first, second) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class B42 : B4 {
    override func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_nolabel(,first, second) // expected-error{{unexpected ',' separator}}
    }
}

class B43 : B4 {
    override func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_nolabel(first, second)
    }
}

class B44 : B4 {
    override func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_nolabel(first,, second) // expected-error{{unexpected ',' separator}}
    }
}

class B45 : B4 {
    override func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_nolabel(first, second,)
    }
}

class B46 : B4 {
    override func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_nolabel(first, second,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class B47 : B4 {
    override func twoarg__firstarg_nolabel__secondarg_nolabel(_ first: Int, _ second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_nolabel(first,, second,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class B5 {
    func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {}
}

class B51 : B5 {
    override func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {
        super.twoarg__firstarg_label__secondarg_nolabel(,,first: first, second) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class B52 : B5 {
    override func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {
        super.twoarg__firstarg_label__secondarg_nolabel(,first: first, second) // expected-error{{unexpected ',' separator}}
    }
}

class B53 : B5 {
    override func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {
        super.twoarg__firstarg_label__secondarg_nolabel(first: first, second)
    }
}

class B54 : B5 {
    override func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {
        super.twoarg__firstarg_label__secondarg_nolabel(first: first,, second) // expected-error{{unexpected ',' separator}}
    }
}

class B55 : B5 {
    override func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {
        super.twoarg__firstarg_label__secondarg_nolabel(first: first, second,)
    }
}

class B56 : B5 {
    override func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {
        super.twoarg__firstarg_label__secondarg_nolabel(first: first,, second,) // expected-error{{unexpected ',' separator}}
    }
}

class B57 : B5 {
    override func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {
        super.twoarg__firstarg_label__secondarg_nolabel(first: first, second,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class B58 : B5 {
    override func twoarg__firstarg_label__secondarg_nolabel(first: Int, _ second: Int) {
        super.twoarg__firstarg_label__secondarg_nolabel(first: first,, second,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class B6 {
    func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {}
}

class B61 : B6 {
    override func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_label(,,first, second: second) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class B62 : B6 {
    override func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_label(,first, second: second) // expected-error{{unexpected ',' separator}}
    }
}

class B63 : B6 {
    override func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_label(first, second: second)
    }
}

class B64 : B6 {
    override func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_label(first,, second: second) // expected-error{{unexpected ',' separator}}
    }
}

class B65 : B6 {
    override func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_label(first, second: second,)
    }
}

class B66 : B6 {
    override func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_label(first,, second: second,) // expected-error{{unexpected ',' separator}}
    }
}

class B67 : B6 {
    override func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_label(first, second: second,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class B68 : B6 {
    override func twoarg__firstarg_nolabel__secondarg_label(_ first: Int, second: Int) {
        super.twoarg__firstarg_nolabel__secondarg_label(first,, second: second,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class B7 {
    func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {}
}

class B71 : B7 {
    override func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {
        super.twoarg__firstarg_label__secondarg_label(,,first: first, second: second) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class B72 : B7 {
    override func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {
        super.twoarg__firstarg_label__secondarg_label(,first: first, second: second) // expected-error{{unexpected ',' separator}}
    }
}

class B73 : B7 {
    override func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {
        super.twoarg__firstarg_label__secondarg_label(first: first, second: second)
    }
}

class B74 : B7 {
    override func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {
        super.twoarg__firstarg_label__secondarg_label(first: first,, second: second) // expected-error{{unexpected ',' separator}}
    }
}

class B75 : B7 {
    override func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {
        super.twoarg__firstarg_label__secondarg_label(first: first, second: second,)
    }
}

class B76 : B7 {
    override func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {
        super.twoarg__firstarg_label__secondarg_label(first: first,, second: second,) // expected-error{{unexpected ',' separator}}
    }
}

class B77 : B7 {
    override func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {
        super.twoarg__firstarg_label__secondarg_label(first: first, second: second,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class B78 : B7 {
    override func twoarg__firstarg_label__secondarg_label(first: Int, second: Int) {
        super.twoarg__firstarg_label__secondarg_label(first: first,, second: second,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class BS1 {
    subscript() -> Int { get { return 0 } set {} }
}

class BS11 : BS1 {
    override subscript() -> Int { 
        get { return super[] } 
        set { super[] = newValue } 
    }
}

class BS12 : BS1 {
    override subscript() -> Int { 
        get { return super[,] }  // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    } 
}

class BS13 : BS1 {
    override subscript() -> Int { 
        get { return super[,,] }  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    } 
}


class BS2 {
    subscript(first: Int) -> Int { get { return 0 } set {} }
}

class BS21 : BS2 {
    override subscript(first: Int) -> Int {
        get { return super[,,-2] }  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
        set { super[,,-2] = newValue }  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class BS22 : BS2 {
    override subscript(first: Int) -> Int {
        get { return super[,-1] }  // expected-error{{unexpected ',' separator}}
        set { super[,-1] = newValue }  // expected-error{{unexpected ',' separator}}
    }
}

class BS23 : BS2 {
    override subscript(first: Int) -> Int {
        get { return super[0] }  
        set { super[0] = newValue } 
    }
}

class BS24 : BS2 {
    override subscript(first: Int) -> Int {
        get { return super[1,] }  
        set { super[1,] = newValue } 
    }
}

class BS25 : BS2 {
    override subscript(first: Int) -> Int {
        get { return super[2,,] }  // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[2,,] = newValue }  // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class BS3 {
    subscript(first first: Int) -> Int { get { return 0 } set {} }
}

class BS31 : BS3 {
    override subscript(first first: Int) -> Int {
        get { return super[,,first: -2] }  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
        set { super[,,first: -2] = newValue }  // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class BS32 : BS3 {
    override subscript(first first: Int) -> Int {
        get { return super[,first: -1] }  // expected-error{{unexpected ',' separator}}
        set { super[,first: -1] = newValue }  // expected-error{{unexpected ',' separator}}
    }
}

class BS33 : BS3 {
    override subscript(first first: Int) -> Int {
        get { return super[first: 0] }  
        set { super[first: 0] = newValue } 
    }
}

class BS34 : BS3 {
    override subscript(first first: Int) -> Int {
        get { return super[first: 1,] }  
        set { super[first: 1,] = newValue } 
    }
}

class BS35 : BS3 {
    override subscript(first first: Int) -> Int {
        get { return super[first: 2,,] }  // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first: 2,,] = newValue }  // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class BS4 {
    subscript(first: Int, second: Int) -> Int { get { return 0 } set {} }
}

class BS41 : BS4 {
    override subscript(first: Int, second: Int) -> Int {
        get { return super[,,first, second] } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
        set { super[,,first, second] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class BS42 : BS4 {
    override subscript(first: Int, second: Int) -> Int {
        get { return super[,first, second] } // expected-error{{unexpected ',' separator}}
        set { super[,first, second] = newValue } // expected-error{{unexpected ',' separator}}
    }
}

class BS43 : BS4 {
    override subscript(first: Int, second: Int) -> Int {
        get { return super[first, second] }
        set { super[first, second] = newValue }
    }
}

class BS44 : BS4 {
    override subscript(first: Int, second: Int) -> Int {
        get { return super[first,, second] } // expected-error{{unexpected ',' separator}}
        set { super[first,, second] = newValue } // expected-error{{unexpected ',' separator}}
    }
}

class BS45 : BS4 {
    override subscript(first: Int, second: Int) -> Int {
        get { return super[first, second,] }
        set { super[first, second,] = newValue }
    }
}

class BS46 : BS4 {
    override subscript(first: Int, second: Int) -> Int {
        get { return super[first, second,,] } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first, second,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class BS47 : BS4 {
    override subscript(first: Int, second: Int) -> Int {
        get { return super[first,, second,,] } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first,, second,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class BS5 {
    subscript(first first: Int, second: Int) -> Int { get { return 0 } set {} }
}

class BS51 : BS5 {
    override subscript(first first: Int, second: Int) -> Int {
        get { return super[,,first: first, second] } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
        set { super[,,first: first, second] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class BS52 : BS5 {
    override subscript(first first: Int, second: Int) -> Int {
        get { return super[,first: first, second] } // expected-error{{unexpected ',' separator}}
        set { super[,first: first, second] = newValue } // expected-error{{unexpected ',' separator}}
    }
}

class BS53 : BS5 {
    override subscript(first first: Int, second: Int) -> Int {
        get { return super[first: first, second] }
        set { super[first: first, second] = newValue }
    }
}

class BS54 : BS5 {
    override subscript(first first: Int, second: Int) -> Int {
        get { return super[first: first,, second] } // expected-error{{unexpected ',' separator}}
        set { super[first: first,, second] = newValue } // expected-error{{unexpected ',' separator}}
    }
}

class BS55 : BS5 {
    override subscript(first first: Int, second: Int) -> Int {
        get { return super[first: first, second,] }
        set { super[first: first, second,] = newValue }
    }
}

class BS56 : BS5 {
    override subscript(first first: Int, second: Int) -> Int {
        get { return super[first: first, second,,] } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first: first, second,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class BS57 : BS5 {
    override subscript(first first: Int, second: Int) -> Int {
        get { return super[first: first,, second,,] } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first: first,, second,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class BS6 {
    subscript(first: Int, second second: Int) -> Int { get { return 0 } set {} }
}

class BS61 : BS6 {
    override subscript(first: Int, second second: Int) -> Int {
        get { return super[,,first, second: second] } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
        set { super[,,first, second: second] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class BS62 : BS6 {
    override subscript(first: Int, second second: Int) -> Int {
        get { return super[,first, second: second] } // expected-error{{unexpected ',' separator}}
        set { super[,first, second: second] = newValue } // expected-error{{unexpected ',' separator}}
    }
}

class BS63 : BS6 {
    override subscript(first: Int, second second: Int) -> Int {
        get { return super[first, second: second] }
        set { super[first, second: second] = newValue }
    }
}

class BS64 : BS6 {
    override subscript(first: Int, second second: Int) -> Int {
        get { return super[first,, second: second] } // expected-error{{unexpected ',' separator}}
        set { super[first,, second: second] = newValue } // expected-error{{unexpected ',' separator}}
    }
}

class BS65 : BS6 {
    override subscript(first: Int, second second: Int) -> Int {
        get { return super[first, second: second,] }
        set { super[first, second: second,] = newValue }
    }
}

class BS66 : BS6 {
    override subscript(first: Int, second second: Int) -> Int {
        get { return super[first, second: second,,] } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first, second: second,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class BS67 : BS6 {
    override subscript(first: Int, second second: Int) -> Int {
        get { return super[first,, second: second,,] } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first,, second: second,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


class BS7 {
    subscript(first first: Int, second second: Int) -> Int { get { return 0 } set {} }
}

class BS71 : BS7 {
    override subscript(first first: Int, second second: Int) -> Int {
        get { return super[,,first: first, second: second] } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
        set { super[,,first: first, second: second] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
    }
}

class BS72 : BS7 {
    override subscript(first first: Int, second second: Int) -> Int {
        get { return super[,first: first, second: second] } // expected-error{{unexpected ',' separator}}
        set { super[,first: first, second: second] = newValue } // expected-error{{unexpected ',' separator}}
    }
}

class BS73 : BS7 {
    override subscript(first first: Int, second second: Int) -> Int {
        get { return super[first: first, second: second] }
        set { super[first: first, second: second] = newValue }
    }
}

class BS74 : BS7 {
    override subscript(first first: Int, second second: Int) -> Int {
        get { return super[first: first,, second: second] } // expected-error{{unexpected ',' separator}}
        set { super[first: first,, second: second] = newValue } // expected-error{{unexpected ',' separator}}
    }
}

class BS75 : BS7 {
    override subscript(first first: Int, second second: Int) -> Int {
        get { return super[first: first, second: second,] }
        set { super[first: first, second: second,] = newValue }
    }
}

class BS76 : BS7 {
    override subscript(first first: Int, second second: Int) -> Int {
        get { return super[first: first, second: second,,] } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first: first, second: second,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}

class BS77 : BS7 {
    override subscript(first first: Int, second second: Int) -> Int {
        get { return super[first: first,, second: second,,] } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
        set { super[first: first,, second: second,,] = newValue } // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
    }
}


enum E {
    case zero
    case one_nolabel(Int)
    case one_label(label: Int)
    case two__first_nolabel__second_nolabel(Int, Int)
    case two__first_label__second_nolabel(first: Int, Int)
    case two__first_nolabel__second_label(Int, second: Int)
    case two__first_label__second_label(first: Int, second: Int)
}

let e01: E = .zero
let e02: E = .zero(,) // expected-error{{enum case 'zero' has no associated values}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let e03: E = .zero(,,) // expected-error{{enum case 'zero' has no associated values}} expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let e11 = E.one_nolabel(,,-2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let e12 = E.one_nolabel(,-1) // expected-error{{unexpected ',' separator}}
let e13 = E.one_nolabel(0)
let e14 = E.one_nolabel(1,)
let e15 = E.one_nolabel(2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let e21 = E.one_label(,,label: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let e22 = E.one_label(,label: -1) // expected-error{{unexpected ',' separator}}
let e23 = E.one_label(label: 0)
let e24 = E.one_label(label: 1,)
let e25 = E.one_label(label: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let e31 = E.two__first_nolabel__second_nolabel(,,-2, -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let e32 = E.two__first_nolabel__second_nolabel(,-1, -1) // expected-error{{unexpected ',' separator}}
let e33 = E.two__first_nolabel__second_nolabel(0, 0)
let e34 = E.two__first_nolabel__second_nolabel(0,, 0) // expected-error{{unexpected ',' separator}}
let e35 = E.two__first_nolabel__second_nolabel(1, 1,)
let e36 = E.two__first_nolabel__second_nolabel(1,, 1,) // expected-error{{unexpected ',' separator}}
let e37 = E.two__first_nolabel__second_nolabel(2, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let e38 = E.two__first_nolabel__second_nolabel(2,, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let e41 = E.two__first_label__second_nolabel(,,first: -2, -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let e42 = E.two__first_label__second_nolabel(,first: -1, -1) // expected-error{{unexpected ',' separator}}
let e43 = E.two__first_label__second_nolabel(first: 0, 0)
let e44 = E.two__first_label__second_nolabel(first: 0,, 0) // expected-error{{unexpected ',' separator}}
let e45 = E.two__first_label__second_nolabel(first: 1, 1,)
let e46 = E.two__first_label__second_nolabel(first: 1,, 1,) // expected-error{{unexpected ',' separator}}
let e47 = E.two__first_label__second_nolabel(first: 2, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let e48 = E.two__first_label__second_nolabel(first: 2,, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let e51 = E.two__first_nolabel__second_label(,,-2, second: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let e52 = E.two__first_nolabel__second_label(,-1, second: -1) // expected-error{{unexpected ',' separator}}
let e53 = E.two__first_nolabel__second_label(0, second: 0)
let e54 = E.two__first_nolabel__second_label(0,, second: 0) // expected-error{{unexpected ',' separator}}
let e55 = E.two__first_nolabel__second_label(1, second: 1,)
let e56 = E.two__first_nolabel__second_label(1,, second: 1,) // expected-error{{unexpected ',' separator}}
let e57 = E.two__first_nolabel__second_label(2, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let e58 = E.two__first_nolabel__second_label(2,, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let e61 = E.two__first_label__second_label(,,first: -2, second: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let e62 = E.two__first_label__second_label(,first: -1, second: -1) // expected-error{{unexpected ',' separator}}
let e63 = E.two__first_label__second_label(first: 0, second: 0)
let e64 = E.two__first_label__second_label(first: 0,, second: 0) // expected-error{{unexpected ',' separator}}
let e65 = E.two__first_label__second_label(first: 1, second: 1,)
let e66 = E.two__first_label__second_label(first: 1,, second: 1,) // expected-error{{unexpected ',' separator}}
let e67 = E.two__first_label__second_label(first: 2, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let e68 = E.two__first_label__second_label(first: 2,, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


let t01: Void = ()
let t02: Void = (,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let t03: Void = (,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let t11: (Int) = (,,-2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let t12: (Int) = (,-1) // expected-error{{unexpected ',' separator}}
let t13: (Int) = (0)
let t14: (Int) = (1,)
let t15: (Int) = (2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let t21: (label: Int) = (,,label: -2) // expected-error{{unexpected ',' separator}} expected-error{{cannot create a single-element tuple with an element label}} expected-error{{cannot convert value of type '(label: Int)' to specified type 'Int'}} expected-error{{unexpected ',' separator}}
let t22: (label: Int) = (,label: -1) // expected-error{{unexpected ',' separator}} expected-error{{cannot create a single-element tuple with an element label}} expected-error{{cannot convert value of type '(label: Int)' to specified type 'Int'}}
let t23: (label: Int) = (label: 0) // expected-error{{cannot create a single-element tuple with an element label}} expected-error{{cannot convert value of type '(label: Int)' to specified type 'Int'}}
let t24: (label: Int) = (label: 1,) // expected-error{{cannot create a single-element tuple with an element label}} expected-error{{cannot convert value of type '(label: Int)' to specified type 'Int'}}
let t25: (label: Int) = (label: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{cannot create a single-element tuple with an element label}} expected-error{{cannot convert value of type '(label: Int)' to specified type 'Int'}} expected-error{{expected expression in list of expressions}}

let t31: (Int, Int) = (,,-2, -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let t32: (Int, Int) = (,-1, -1) // expected-error{{unexpected ',' separator}}
let t33: (Int, Int) = (0, 0)
let t34: (Int, Int) = (0,, 0) // expected-error{{unexpected ',' separator}}
let t35: (Int, Int) = (1, 1,)
let t36: (Int, Int) = (1,, 1,) // expected-error{{unexpected ',' separator}}
let t37: (Int, Int) = (2, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let t38: (Int, Int) = (2,, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let t41: (first: Int, Int) = (,,first: -2, -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let t42: (first: Int, Int) = (,first: -1, -1) // expected-error{{unexpected ',' separator}}
let t43: (first: Int, Int) = (first: 0, 0)
let t44: (first: Int, Int) = (first: 0,, 0) // expected-error{{unexpected ',' separator}}
let t45: (first: Int, Int) = (first: 1, 1,)
let t46: (first: Int, Int) = (first: 1,, 1,) // expected-error{{unexpected ',' separator}}
let t47: (first: Int, Int) = (first: 2, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let t48: (first: Int, Int) = (first: 2,, 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let t51: (Int, second: Int) = (,,-2, second: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let t52: (Int, second: Int) = (,-1, second: -1) // expected-error{{unexpected ',' separator}}
let t53: (Int, second: Int) = (0, second: 0)
let t54: (Int, second: Int) = (0,, second: 0) // expected-error{{unexpected ',' separator}}
let t55: (Int, second: Int) = (1, second: 1,)
let t56: (Int, second: Int) = (1,, second: 1,) // expected-error{{unexpected ',' separator}}
let t57: (Int, second: Int) = (2, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let t58: (Int, second: Int) = (2,, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}

let t61: (first: Int, second: Int) = (,,first: -2, second: -2) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let t62: (first: Int, second: Int) = (,first: -1, second: -1) // expected-error{{unexpected ',' separator}}
let t63: (first: Int, second: Int) = (first: 0, second: 0)
let t64: (first: Int, second: Int) = (first: 0,, second: 0) // expected-error{{unexpected ',' separator}}
let t65: (first: Int, second: Int) = (first: 1, second: 1,)
let t66: (first: Int, second: Int) = (first: 1,, second: 1,) // expected-error{{unexpected ',' separator}}
let t67: (first: Int, second: Int) = (first: 2, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}
let t68: (first: Int, second: Int) = (first: 2,, second: 2,,) // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}}


struct SS {
    subscript() -> Int { get { return 0 } set {} }
    subscript(first: Int) -> Int { get { return 0 } set {} }
    subscript(first first: Int) -> Int { get { return 0 } set {} }
    subscript(first: Int, second: Int) -> Int { get { return 0 } set {} }
    subscript(first first: Int, second: Int) -> Int { get { return 0 } set {} }
    subscript(first: Int, second second: Int) -> Int { get { return 0 } set {} }
    subscript(first first: Int, second second: Int) -> Int { get { return 0 } set {} }
}

let kp01 = \SS[]
let kp02 = \SS[,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}
let kp03 = \SS[,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}

let kp11 = \SS[,,-2] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let kp12 = \SS[,-1] // expected-error{{unexpected ',' separator}}
let kp13 = \SS[0]
let kp14 = \SS[1,]
let kp15 = \SS[2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}

let kp21 = \SS[,,first: -2] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let kp22 = \SS[,first: -1] // expected-error{{unexpected ',' separator}}
let kp23 = \SS[first: 0]
let kp24 = \SS[first: 1,]
let kp25 = \SS[first: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}

let kp31 = \SS[,,-2, -2] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let kp32 = \SS[,-1, -1] //  expected-error{{unexpected ',' separator}}
let kp33 = \SS[0, 0]
let kp34 = \SS[0,, 0] // expected-error{{unexpected ',' separator}}
let kp35 = \SS[1, 1,]
let kp36 = \SS[1,, 1,] // expected-error{{unexpected ',' separator}}
let kp37 = \SS[2, 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}
let kp38 = \SS[2,, 2,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}

let kp41 = \SS[,,first: -2, -2] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let kp42 = \SS[,first: -1, -1] // expected-error{{unexpected ',' separator}}
let kp43 = \SS[first: 0, 0]
let kp44 = \SS[first: 0,, 0] // expected-error{{unexpected ',' separator}}
let kp45 = \SS[first: 1, 1,]
let kp46 = \SS[first: 1,, 1,] // expected-error{{unexpected ',' separator}}
let kp47 = \SS[first: 2, 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}
let kp48 = \SS[first: 2,, 2,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}

let kp51 = \SS[,,-2, second: -2] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let kp52 = \SS[,-1, second: -1] // expected-error{{unexpected ',' separator}}
let kp53 = \SS[0, second: 0]
let kp54 = \SS[0,, second: 0] // expected-error{{unexpected ',' separator}}
let kp55 = \SS[1, second: 1,]
let kp56 = \SS[1,, second: 1,] // expected-error{{unexpected ',' separator}}
let kp57 = \SS[2, second: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}
let kp58 = \SS[2,, second: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}

let kp61 = \SS[,,first: -2, second: -2] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}}
let kp62 = \SS[,first: -1, second: -1] // expected-error{{unexpected ',' separator}}
let kp63 = \SS[first: 0, second: 0]
let kp64 = \SS[first: 0,, second: 0] // expected-error{{unexpected ',' separator}}
let kp65 = \SS[first: 1, second: 1,]
let kp66 = \SS[first: 1,, second: 1,] // expected-error{{unexpected ',' separator}}
let kp67 = \SS[first: 2, second: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}
let kp68 = \SS[first: 2,, second: 2,,] // expected-error{{unexpected ',' separator}} expected-error{{unexpected ',' separator}} expected-error{{expected expression in list of expressions}} expected-error{{instance member 'subscript' cannot be used on type 'SS'}}

