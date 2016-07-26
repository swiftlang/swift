// RUN: %target-parse-verify-swift

// Conflict marker parsing should never conflict with operator parsing.

prefix operator <<<<<<<
infix operator <<<<<<<

prefix func <<<<<<< (x : String) {}
func <<<<<<< (x : String, y : String) {}

prefix operator >>>>>>>
infix operator >>>>>>>

prefix func >>>>>>> (x : String) {}
func >>>>>>> (x : String, y : String) {}

// diff3-style conflict markers

<<<<<<< HEAD:conflict_markers.swift // expected-error {{source control conflict marker in source file}}
var a : String = "A"
var b : String = "b"
=======
var a : String = "a"
var b : String = "B"
>>>>>>> 18844bc65229786b96b89a9fc7739c0fc897905e:conflict_markers.swift
print(a + b) // expected-error {{use of unresolved identifier 'a'}} expected-error {{use of unresolved identifier 'b'}}

<<<<<<< HEAD:conflict_markers.swift // expected-error {{source control conflict marker in source file}}
======= 
var d : String = "D"
>>>>>>> 18844bc65229786b96b89a9fc7739c0fc897905e:conflict_markers.swift
print(d) // expected-error {{use of unresolved identifier 'd'}}

<<<<<<<"HEAD:fake_conflict_markers.swift" // No error
>>>>>>>"18844bc65229786b96b89a9fc7739c0fc897905e:fake_conflict_markers.swift" // No error

<<<<<<< HEAD:conflict_markers.swift // expected-error {{source control conflict marker in source file}}
<<<<<<<"HEAD:fake_conflict_markers.swift"
var fake_b : String = "a"
>>>>>>>"18844bc65229786b96b89a9fc7739c0fc897905e:fake_conflict_markers.swift"
=======
<<<<<<<"HEAD:fake_conflict_markers.swift"
var fake_c : String = "a"
>>>>>>>"18844bc65229786b96b89a9fc7739c0fc897905e:fake_conflict_markers.swift"
>>>>>>> 18844bc65229786b96b89a9fc7739c0fc897905e:conflict_markers.swift
print(fake_b + fake_c) // expected-error {{use of unresolved identifier 'fake_b'}} expected-error {{use of unresolved identifier 'fake_c'}}

// Disambiguating conflict markers from operator applications.

_ = {
  
// Conflict marker.
  
let a = "a", b = "b"
a // expected-warning {{expression of type 'String' is unused}}
<<<<<<< b // expected-error {{source control conflict marker in source file}}
a
>>>>>>> b
  
// Not a conflict marker.
  
a
  <<<<<<< b
a
  >>>>>>> b
}()
  
// Perforce-style conflict markers

>>>> ORIGINAL // expected-error {{source control conflict marker in source file}}
var a : String = "A"
var b : String = "B"
==== THEIRS
var a : String = "A"
var b : String = "b"
==== YOURS
var a : String = "a"
var b : String = "B"
<<<<
print(a + b) // expected-error {{use of unresolved identifier 'a'}} expected-error {{use of unresolved identifier 'b'}}

>>>> ORIGINAL // expected-error {{source control conflict marker in source file}}
==== THEIRS
==== YOURS
var d : String = "D"
<<<<
print(d) // expected-error {{use of unresolved identifier 'd'}}

