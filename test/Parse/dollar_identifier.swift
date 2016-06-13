// RUN: %target-parse-verify-swift

var $ : Int // expected-error {{expected numeric value following '$'}}
let $ = 42 // expected-error {{expected numeric value following '$'}}
class $ : {} // expected-error {{expected numeric value following '$'}}
enum $ : {} // expected-error {{expected numeric value following '$'}}
struct $ : {} // expected-error {{expected numeric value following '$'}}

var $a : Int // expected-error {{expected numeric value following '$'}}
let $b = 42 // expected-error {{expected numeric value following '$'}}
class $c : {} // expected-error {{expected numeric value following '$'}}
enum $d : {} // expected-error {{expected numeric value following '$'}}
struct $e : {} // expected-error {{expected numeric value following '$'}}
