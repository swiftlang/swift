// RUN: %swift %s -verify

var var_redecl1: Int // expected-note {{'var_redecl1' previously declared here}}
var_redecl1 = 0
var var_redecl1: UInt // expected-error {{invalid redeclaration}}

var var_redecl2: Int // expected-note {{'var_redecl2' previously declared here}}
var_redecl2 = 0
var var_redecl2: Int // expected-error {{invalid redeclaration}}

var var_redecl3: (Int) -> () { get {} } // expected-note {{'var_redecl3' previously declared here}}
var var_redecl3: () -> () { get {} } // expected-error {{invalid redeclaration}}

var var_redecl4: Int // expected-note 2{{'var_redecl4' previously declared here}}
var var_redecl4: Int // expected-error {{invalid redeclaration}} expected-note {{'var_redecl4' previously declared here}}
var var_redecl4: Int // expected-error 2{{invalid redeclaration}}


let let_redecl1: Int = 0 // expected-note {{'let_redecl1' previously declared here}}
let let_redecl1: UInt = 0 // expected-error {{invalid redeclaration}}

let let_redecl2: Int = 0 // expected-note {{'let_redecl2' previously declared here}}
let let_redecl2: Int = 0 // expected-error {{invalid redeclaration}}


class class_redecl1 {} // expected-note {{'class_redecl1' previously declared here}}
class class_redecl1 {} // expected-error {{invalid redeclaration}}

class class_redecl2<T> {} // expected-note {{'class_redecl2' previously declared here}}
class class_redecl2 {} // expected-error {{invalid redeclaration}}

class class_redecl3 {} // expected-note {{'class_redecl3' previously declared here}}
class class_redecl3<T> {} // expected-error {{invalid redeclaration}}


struct struct_redecl1 {} // expected-note {{'struct_redecl1' previously declared here}}
struct struct_redecl1 {} // expected-error {{invalid redeclaration}}

struct struct_redecl2<T> {} // expected-note {{'struct_redecl2' previously declared here}}
struct struct_redecl2 {} // expected-error {{invalid redeclaration}}

struct struct_redecl3 {} // expected-note {{'struct_redecl3' previously declared here}}
struct struct_redecl3<T> {} // expected-error {{invalid redeclaration}}


enum enum_redecl1 {} // expected-note {{'enum_redecl1' previously declared here}}
enum enum_redecl1 {} // expected-error {{invalid redeclaration}}

enum enum_redecl2<T> {} // expected-note {{'enum_redecl2' previously declared here}}
enum enum_redecl2 {} // expected-error {{invalid redeclaration}}

enum enum_redecl3 {} // expected-note {{'enum_redecl3' previously declared here}}
enum enum_redecl3<T> {} // expected-error {{invalid redeclaration}}


protocol protocol_redecl1 {} // expected-note {{'protocol_redecl1' previously declared here}}
protocol protocol_redecl1 {} // expected-error {{invalid redeclaration}}


typealias typealias_redecl1 = Int // expected-note {{'typealias_redecl1' previously declared here}}
typealias typealias_redecl1 = Int // expected-error {{invalid redeclaration}}

typealias typealias_redecl2 = Int // expected-note {{'typealias_redecl2' previously declared here}}
typealias typealias_redecl2 = UInt // expected-error {{invalid redeclaration}}


var mixed_redecl1: Int // expected-note {{'mixed_redecl1' previously declared here}}
class mixed_redecl1 {} // expected-error {{invalid redeclaration}}

class mixed_redecl2 {} // expected-note {{'mixed_redecl2' previously declared here}}
struct mixed_redecl2 {} // expected-error {{invalid redeclaration}}

class mixed_redecl3 {} // expected-note {{'mixed_redecl3' previously declared here}}
enum mixed_redecl3 {} // expected-error {{invalid redeclaration}}

class mixed_redecl4 {} // expected-note {{'mixed_redecl4' previously declared here}}
protocol mixed_redecl4 {} // expected-error {{invalid redeclaration}}

class mixed_redecl5 {} // expected-note {{'mixed_redecl5' previously declared here}}
typealias mixed_redecl5 = Int // expected-error {{invalid redeclaration}}

func mixed_redecl6() {} // expected-note {{'mixed_redecl6' previously declared here}}
var mixed_redecl6: Int // expected-error {{invalid redeclaration}}

var mixed_redecl7: Int // FIXME-note {{'mixed_redecl7' previously declared here}}
func mixed_redecl7() {} // FIXME-error {{invalid redeclaration}}

func mixed_redecl8() {} // expected-note {{'mixed_redecl8' previously declared here}}
class mixed_redecl8 {} // expected-error {{invalid redeclaration}}

class mixed_redecl9 {} // FIXME-note {{'mixed_redecl9' previously declared here}}
func mixed_redecl9() {} // FIXME-error {{invalid redeclaration}}

func mixed_redecl10() {} // expected-note {{'mixed_redecl10' previously declared here}}
typealias mixed_redecl10 = Int // expected-error {{invalid redeclaration}}

typealias mixed_redecl11 = Int // FIXME-note {{'mixed_redecl11' previously declared here}}
func mixed_redecl11() {} // FIXME-error {{invalid redeclaration}}

var mixed_redecl12: Int // expected-note {{'mixed_redecl12' previously declared here}}
let mixed_redecl12: Int = 0 // expected-error {{invalid redeclaration}}

let mixed_redecl13: Int = 0 // expected-note {{'mixed_redecl13' previously declared here}}
var mixed_redecl13: Int // expected-error {{invalid redeclaration}}

