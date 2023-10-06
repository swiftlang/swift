struct A<T> {
    init<F>(_ f: F) {}
    func foo<E>(_ e: E) -> E { return e }
}
let a = A<Double>("hello")
let fooResult = a.foo(42)

// RUN: %sourcekitd-test -req=cursor -pos=5:9 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK0 %s
// CHECK0: source.lang.swift.ref.struct (1:8-1:9)
// CHECK0: A
// CHECK0: s:20cursor_info_generics1AV
// CHECK0: source.lang.swift
// CHECK0: A<T>.Type
// CHECK0: $s20cursor_info_generics1AVyxGmD
// CHECK0: cursor_info_generics
// CHECK0: <Declaration>struct A&lt;T&gt;</Declaration>
// CHECK0: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>A</decl.name>&lt;<decl.generic_type_param usr="s:20cursor_info_generics1AV1Txmfp"><decl.generic_type_param.name>T</decl.generic_type_param.name></decl.generic_type_param>&gt;</decl.struct>
// CHECK0: SUBSTITUTIONS BEGIN
// CHECK-NEXT0: T -> Double (s:20cursor_info_generics1AV1Txmfp -> s:Sd)
// CHECK-NEXT0: SUBSTITUTIONS END
// CHECK0: SECONDARY SYMBOLS BEGIN
// CHECK0: source.lang.swift.ref.function.constructor (2:5-2:20)
// CHECK0: init(_:)
// CHECK0: s:20cursor_info_generics1AVyACyxGqd__clufc
// CHECK0: source.lang.swift
// CHECK0: <T, F> (A<T>.Type) -> (F) -> A<T>
// CHECK0: $sy20cursor_info_generics1AVyxGqd__cluD
// CHECK0: cursor_info_generics
// CHECK0: <Declaration>init&lt;F&gt;(_ f: <Type usr="s:20cursor_info_generics1AVyACyxGqd__clufc1FL_qd__mfp">F</Type>)</Declaration>
// CHECK0: <decl.function.constructor><syntaxtype.keyword>init</syntaxtype.keyword>&lt;<decl.generic_type_param usr="s:20cursor_info_generics1AVyACyxGqd__clufc1FL_qd__mfp"><decl.generic_type_param.name>F</decl.generic_type_param.name></decl.generic_type_param>&gt;(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>f</decl.var.parameter.name>: <decl.var.parameter.type><ref.generic_type_param usr="s:20cursor_info_generics1AVyACyxGqd__clufc1FL_qd__mfp">F</ref.generic_type_param></decl.var.parameter.type></decl.var.parameter>)</decl.function.constructor>
// CHECK0: SUBSTITUTIONS BEGIN
// CHECK-NEXT0: T -> Double (s:20cursor_info_generics1AV1Txmfp -> s:Sd)
// CHECK-NEXT0: F -> String (s:20cursor_info_generics1AVyACyxGqd__clufc1FL_qd__mfp -> s:SS)
// CHECK-NEXT0: SUBSTITUTIONS END
// CHECK0: -----
// CHECK0: source.lang.swift.ref.struct ()
// CHECK0: Double
// CHECK0: s:Sd
// CHECK0: source.lang.swift
// CHECK0: Double.Type
// CHECK0: $sSdmD
// CHECK0: Swift
// CHECK0: <Group>Math/Floating</Group>
// CHECK0: SYSTEM
// CHECK0: <Declaration>@frozen struct Double</Declaration>
// CHECK0: <decl.struct><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@frozen</syntaxtype.attribute.name></syntaxtype.attribute.builtin> <syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>Double</decl.name></decl.struct>
// CHECK0: <Class><Name>Double</Name><USR>s:Sd</USR><Declaration>@frozen struct Double</Declaration><CommentParts><Abstract><Para>A double-precision, floating-point value type.</Para></Abstract></CommentParts></Class>
// CHECK-NEXT0: SUBSTITUTIONS BEGIN
// CHECK-NEXT0: SUBSTITUTIONS END
// CHECK0: -----
// CHECK0: source.lang.swift.ref.generic_type_param (1:10-1:11)
// CHECK0: T
// CHECK0: s:20cursor_info_generics1AV1Txmfp
// CHECK0: source.lang.swift
// CHECK0: T.Type
// CHECK0: $sxmD
// CHECK0: cursor_info_generics
// CHECK0: <Declaration>T</Declaration>
// CHECK0: <decl.generic_type_param><decl.generic_type_param.name>T</decl.generic_type_param.name></decl.generic_type_param>
// CHECK-NEXT0: SUBSTITUTIONS BEGIN
// CHECK-NEXT0: SUBSTITUTIONS END
// CHECK0: -----
// CHECK0: source.lang.swift.ref.struct ()
// CHECK0: String
// CHECK0: s:SS
// CHECK0: source.lang.swift
// CHECK0: String.Type
// CHECK0: $sSSmD
// CHECK0: Swift
// CHECK0: <Group>String</Group>
// CHECK0: SYSTEM
// CHECK0: <Declaration>@frozen @_eagerMove struct String</Declaration>
// CHECK0: <decl.struct><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@frozen</syntaxtype.attribute.name></syntaxtype.attribute.builtin> <syntaxtype.attribute.builtin><syntaxtype.attribute.name>@_eagerMove</syntaxtype.attribute.name></syntaxtype.attribute.builtin> <syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>String</decl.name></decl.struct>
// CHECK0: <Class><Name>String</Name><USR>s:SS</USR><Declaration>@frozen struct String</Declaration><CommentParts><Abstract><Para>A Unicode string value that is a collection of characters.</Para></Abstract><Discussion><Para>A string is a series of characters, such as <codeVoice>&quot;Swift&quot;</codeVoice>, that forms a collection. Strings in Swift are Unicode correct and locale insensitive, and are designed to be efficient. The <codeVoice>String</codeVoice> type bridges with the Objective-C class <codeVoice>NSString</codeVoice> and offers interoperability with C functions that works with strings.</Para><Para>You can create new strings using string literals or string interpolations. A <emphasis>string literal</emphasis> is a series of characters enclosed in quotes.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let greeting = "Welcome!"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para><emphasis>String interpolations</emphasis> are string literals that evaluate any included expressions and convert the results to string form. String interpolations give you an easy way to build a string from multiple pieces. Wrap each expression in a string interpolation in parentheses, prefixed by a backslash.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let name = "Rosa"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let personalizedGreeting = "Welcome, \(name)!"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// personalizedGreeting == "Welcome, Rosa!"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let price = 2]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let number = 3]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let cookiePrice = "\(number) cookies: $\(price * number)."]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// cookiePrice == "3 cookies: $6."]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>Combine strings using the concatenation operator (<codeVoice>+</codeVoice>).</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let longerGreeting = greeting + " We're glad you're here!"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// longerGreeting == "Welcome! We're glad you're here!"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>Multiline string literals are enclosed in three double quotation marks (<codeVoice>&quot;&quot;&quot;</codeVoice>), with each delimiter on its own line. Indentation is stripped from each line of a multiline string literal to match the indentation of the closing delimiter.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let banner = """]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[          __,]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[         (           o  /) _/_]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[          `.  , , , ,  //  /]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[        (___)(_(_/_(_ //_ (__]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[                     /)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[                    (/]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[        """]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><rawHTML><![CDATA[<h1>]]></rawHTML>Modifying and Comparing Strings<rawHTML><![CDATA[</h1>]]></rawHTML><Para>Strings always have value semantics. Modifying a copy of a string leaves the original unaffected.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[var otherGreeting = greeting]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[otherGreeting += " Have a nice time!"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// otherGreeting == "Welcome! Have a nice time!"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(greeting)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "Welcome!"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>Comparing strings for equality using the equal-to operator (<codeVoice>==</codeVoice>) or a relational operator (like <codeVoice>&lt;</codeVoice> or <codeVoice>&gt;=</codeVoice>) is always performed using Unicode canonical representation. As a result, different representations of a string compare as being equal.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let cafe1 = "Cafe\u{301}"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let cafe2 = "Café"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(cafe1 == cafe2)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "true"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>The Unicode scalar value <codeVoice>&quot;\u{301}&quot;</codeVoice> modifies the preceding character to include an accent, so <codeVoice>&quot;e\u{301}&quot;</codeVoice> has the same canonical representation as the single Unicode scalar value <codeVoice>&quot;é&quot;</codeVoice>.</Para><Para>Basic string operations are not sensitive to locale settings, ensuring that string comparisons and other operations always have a single, stable result, allowing strings to be used as keys in <codeVoice>Dictionary</codeVoice> instances and for other purposes.</Para><rawHTML><![CDATA[<h1>]]></rawHTML>Accessing String Elements<rawHTML><![CDATA[</h1>]]></rawHTML><Para>A string is a collection of <emphasis>extended grapheme clusters</emphasis>, which approximate human-readable characters. Many individual characters, such as “é”, “김”, and “🇮🇳”, can be made up of multiple Unicode scalar values. These scalar values are combined by Unicode’s boundary algorithms into extended grapheme clusters, represented by the Swift <codeVoice>Character</codeVoice> type. Each element of a string is represented by a <codeVoice>Character</codeVoice> instance.</Para><Para>For example, to retrieve the first word of a longer string, you can search for a space and then create a substring from a prefix of the string up to that point:</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let name = "Marie Curie"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let firstSpace = name.firstIndex(of: " ") ?? name.endIndex]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let firstName = name[..<firstSpace]]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// firstName == "Marie"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>The <codeVoice>firstName</codeVoice> constant is an instance of the <codeVoice>Substring</codeVoice> type—a type that represents substrings of a string while sharing the original string’s storage. Substrings present the same interface as strings.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[print("\(name)'s first name has \(firstName.count) letters.")]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "Marie Curie's first name has 5 letters."]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><rawHTML><![CDATA[<h1>]]></rawHTML>Accessing a String’s Unicode Representation<rawHTML><![CDATA[</h1>]]></rawHTML><Para>If you need to access the contents of a string as encoded in different Unicode encodings, use one of the string’s <codeVoice>unicodeScalars</codeVoice>, <codeVoice>utf16</codeVoice>, or <codeVoice>utf8</codeVoice> properties. Each property provides access to a view of the string as a series of code units, each encoded in a different Unicode encoding.</Para><Para>To demonstrate the different views available for every string, the following examples use this <codeVoice>String</codeVoice> instance:</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let cafe = "Cafe\u{301} du 🌍"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(cafe)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "Café du 🌍"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>The <codeVoice>cafe</codeVoice> string is a collection of the nine characters that are visible when the string is displayed.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[print(cafe.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "9"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(Array(cafe))]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "["C", "a", "f", "é", " ", "d", "u", " ", "🌍"]"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><rawHTML><![CDATA[<h2>]]></rawHTML>Unicode Scalar View<rawHTML><![CDATA[</h2>]]></rawHTML><Para>A string’s <codeVoice>unicodeScalars</codeVoice> property is a collection of Unicode scalar values, the 21-bit codes that are the basic unit of Unicode. Each scalar value is represented by a <codeVoice>Unicode.Scalar</codeVoice> instance and is equivalent to a UTF-32 code unit.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[print(cafe.unicodeScalars.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "10"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(Array(cafe.unicodeScalars))]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "["C", "a", "f", "e", "\u{0301}", " ", "d", "u", " ", "\u{0001F30D}"]"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(cafe.unicodeScalars.map { $0.value })]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "[67, 97, 102, 101, 769, 32, 100, 117, 32, 127757]"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>The <codeVoice>unicodeScalars</codeVoice> view’s elements comprise each Unicode scalar value in the <codeVoice>cafe</codeVoice> string. In particular, because <codeVoice>cafe</codeVoice> was declared using the decomposed form of the <codeVoice>&quot;é&quot;</codeVoice> character, <codeVoice>unicodeScalars</codeVoice> contains the scalar values for both the letter <codeVoice>&quot;e&quot;</codeVoice> (101) and the accent character <codeVoice>&quot;´&quot;</codeVoice> (769).</Para><rawHTML><![CDATA[<h2>]]></rawHTML>UTF-16 View<rawHTML><![CDATA[</h2>]]></rawHTML><Para>A string’s <codeVoice>utf16</codeVoice> property is a collection of UTF-16 code units, the 16-bit encoding form of the string’s Unicode scalar values. Each code unit is stored as a <codeVoice>UInt16</codeVoice> instance.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[print(cafe.utf16.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "11"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(Array(cafe.utf16))]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "[67, 97, 102, 101, 769, 32, 100, 117, 32, 55356, 57101]"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>The elements of the <codeVoice>utf16</codeVoice> view are the code units for the string when encoded in UTF-16. These elements match those accessed through indexed <codeVoice>NSString</codeVoice> APIs.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let nscafe = cafe as NSString]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(nscafe.length)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "11"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(nscafe.character(at: 3))]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "101"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><rawHTML><![CDATA[<h2>]]></rawHTML>UTF-8 View<rawHTML><![CDATA[</h2>]]></rawHTML><Para>A string’s <codeVoice>utf8</codeVoice> property is a collection of UTF-8 code units, the 8-bit encoding form of the string’s Unicode scalar values. Each code unit is stored as a <codeVoice>UInt8</codeVoice> instance.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[print(cafe.utf8.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "14"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(Array(cafe.utf8))]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "[67, 97, 102, 101, 204, 129, 32, 100, 117, 32, 240, 159, 140, 141]"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>The elements of the <codeVoice>utf8</codeVoice> view are the code units for the string when encoded in UTF-8. This representation matches the one used when <codeVoice>String</codeVoice> instances are passed to C APIs.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let cLength = strlen(cafe)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(cLength)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "14"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><rawHTML><![CDATA[<h1>]]></rawHTML>Measuring the Length of a String<rawHTML><![CDATA[</h1>]]></rawHTML><Para>When you need to know the length of a string, you must first consider what you’ll use the length for. Are you measuring the number of characters that will be displayed on the screen, or are you measuring the amount of storage needed for the string in a particular encoding? A single string can have greatly differing lengths when measured by its different views.</Para><Para>For example, an ASCII character like the capital letter <emphasis>A</emphasis> is represented by a single element in each of its four views. The Unicode scalar value of <emphasis>A</emphasis> is <codeVoice>65</codeVoice>, which is small enough to fit in a single code unit in both UTF-16 and UTF-8.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let capitalA = "A"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(capitalA.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "1"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(capitalA.unicodeScalars.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "1"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(capitalA.utf16.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "1"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(capitalA.utf8.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "1"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>On the other hand, an emoji flag character is constructed from a pair of Unicode scalar values, like <codeVoice>&quot;\u{1F1F5}&quot;</codeVoice> and <codeVoice>&quot;\u{1F1F7}&quot;</codeVoice>. Each of these scalar values, in turn, is too large to fit into a single UTF-16 or UTF-8 code unit. As a result, each view of the string <codeVoice>&quot;🇵🇷&quot;</codeVoice> reports a different length.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let flag = "🇵🇷"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(flag.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "1"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(flag.unicodeScalars.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "2"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(flag.utf16.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "4"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(flag.utf8.count)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "8"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>To check whether a string is empty, use its <codeVoice>isEmpty</codeVoice> property instead of comparing the length of one of the views to <codeVoice>0</codeVoice>. Unlike with <codeVoice>isEmpty</codeVoice>, calculating a view’s <codeVoice>count</codeVoice> property requires iterating through the elements of the string.</Para><rawHTML><![CDATA[<h1>]]></rawHTML>Accessing String View Elements<rawHTML><![CDATA[</h1>]]></rawHTML><Para>To find individual elements of a string, use the appropriate view for your task. For example, to retrieve the first word of a longer string, you can search the string for a space and then create a new string from a prefix of the string up to that point.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let name = "Marie Curie"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let firstSpace = name.firstIndex(of: " ") ?? name.endIndex]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let firstName = name[..<firstSpace]]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[print(firstName)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "Marie"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>Strings and their views share indices, so you can access the UTF-8 view of the <codeVoice>name</codeVoice> string using the same <codeVoice>firstSpace</codeVoice> index.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[print(Array(name.utf8[..<firstSpace]))]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "[77, 97, 114, 105, 101]"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>Note that an index into one view may not have an exact corresponding position in another view. For example, the <codeVoice>flag</codeVoice> string declared above comprises a single character, but is composed of eight code units when encoded as UTF-8. The following code creates constants for the first and second positions in the <codeVoice>flag.utf8</codeVoice> view. Accessing the <codeVoice>utf8</codeVoice> view with these indices yields the first and second code UTF-8 units.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[let firstCodeUnit = flag.startIndex]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[let secondCodeUnit = flag.utf8.index(after: firstCodeUnit)]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// flag.utf8[firstCodeUnit] == 240]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// flag.utf8[secondCodeUnit] == 159]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>When used to access the elements of the <codeVoice>flag</codeVoice> string itself, however, the <codeVoice>secondCodeUnit</codeVoice> index does not correspond to the position of a specific character. Instead of only accessing the specific UTF-8 code unit, that index is treated as the position of the character at the index’s encoded offset. In the case of <codeVoice>secondCodeUnit</codeVoice>, that character is still the flag itself.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[// flag[firstCodeUnit] == "🇵🇷"]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// flag[secondCodeUnit] == "🇵🇷"]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><Para>If you need to validate that an index from one string’s view corresponds with an exact position in another view, use the index’s <codeVoice>samePosition(in:)</codeVoice> method or the <codeVoice>init(_:within:)</codeVoice> initializer.</Para><CodeListing language="swift"><zCodeLineNumbered><![CDATA[if let exactIndex = secondCodeUnit.samePosition(in: flag) {]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[    print(flag[exactIndex])]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[} else {]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[    print("No exact match for this position.")]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[}]]></zCodeLineNumbered><zCodeLineNumbered><![CDATA[// Prints "No exact match for this position."]]></zCodeLineNumbered><zCodeLineNumbered></zCodeLineNumbered></CodeListing><rawHTML><![CDATA[<h1>]]></rawHTML>Performance Optimizations<rawHTML><![CDATA[</h1>]]></rawHTML><Para>Although strings in Swift have value semantics, strings use a copy-on-write strategy to store their data in a buffer. This buffer can then be shared by different copies of a string. A string’s data is only copied lazily, upon mutation, when more than one string instance is using the same buffer. Therefore, the first in any sequence of mutating operations may cost O(<emphasis>n</emphasis>) time and space.</Para><Para>When a string’s contiguous storage fills up, a new buffer must be allocated and data must be moved to the new storage. String buffers use an exponential growth strategy that makes appending to a string a constant time operation when averaged over many append operations.</Para><rawHTML><![CDATA[<h1>]]></rawHTML>Bridging Between String and NSString<rawHTML><![CDATA[</h1>]]></rawHTML><Para>Any <codeVoice>String</codeVoice> instance can be bridged to <codeVoice>NSString</codeVoice> using the type-cast operator (<codeVoice>as</codeVoice>), and any <codeVoice>String</codeVoice> instance that originates in Objective-C may use an <codeVoice>NSString</codeVoice> instance as its storage. Because any arbitrary subclass of <codeVoice>NSString</codeVoice> can become a <codeVoice>String</codeVoice> instance, there are no guarantees about representation or efficiency when a <codeVoice>String</codeVoice> instance is backed by <codeVoice>NSString</codeVoice> storage. Because <codeVoice>NSString</codeVoice> is immutable, it is just as though the storage was shared by a copy. The first in any sequence of mutating operations causes elements to be copied into unique, contiguous storage which may cost O(<emphasis>n</emphasis>) time and space, where <emphasis>n</emphasis> is the length of the string’s encoded representation (or more, if the underlying <codeVoice>NSString</codeVoice> has unusual performance characteristics).</Para><Para>For more information about the Unicode terms used in this discussion, see the <Link href="http://www.unicode.org/glossary/">Unicode.org glossary</Link>. In particular, this discussion mentions <Link href="http://www.unicode.org/glossary/#extended_grapheme_cluster">extended grapheme clusters</Link>, <Link href="http://www.unicode.org/glossary/#unicode_scalar_value">Unicode scalar values</Link>, and <Link href="http://www.unicode.org/glossary/#canonical_equivalent">canonical equivalence</Link>.</Para></Discussion></CommentParts></Class>
// CHECK-NEXT0: SUBSTITUTIONS BEGIN
// CHECK-NEXT0: SUBSTITUTIONS END
// CHECK0: -----
// CHECK0: source.lang.swift.ref.generic_type_param (2:10-2:11)
// CHECK0: F
// CHECK0: s:20cursor_info_generics1AVyACyxGqd__clufc1FL_qd__mfp
// CHECK0: source.lang.swift
// CHECK0: F.Type
// CHECK0: $sqd__mD
// CHECK0: cursor_info_generics
// CHECK0: <Declaration>F</Declaration>
// CHECK0: <decl.generic_type_param><decl.generic_type_param.name>F</decl.generic_type_param.name></decl.generic_type_param>
// CHECK-NEXT0: SUBSTITUTIONS BEGIN
// CHECK-NEXT0: SUBSTITUTIONS END
// CHECK0: -----
// CHECK0: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=6:19 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: source.lang.swift.ref.function.method.instance (3:10-3:24)
// CHECK1: foo(_:)
// CHECK1: s:20cursor_info_generics1AV3fooyqd__qd__lF
// CHECK1: SUBSTITUTIONS BEGIN
// CHECK-NEXT1: T -> Double (s:20cursor_info_generics1AV1Txmfp -> s:Sd)
// CHECK-NEXT1: E -> Int (s:20cursor_info_generics1AV3fooyqd__qd__lF1EL_qd__mfp -> s:Si)
// CHECK-NEXT1: SUBSTITUTIONS END
// CHECK1: SECONDARY SYMBOLS BEGIN
// CHECK1: source.lang.swift.ref.struct ()
// CHECK1: Double
// CHECK1: s:Sd
// CHECK1: SUBSTITUTIONS BEGIN
// CHECK-NEXT1: SUBSTITUTIONS END
// CHECK1: -----
// CHECK1: source.lang.swift.ref.generic_type_param (1:10-1:11)
// CHECK1: T
// CHECK1: s:20cursor_info_generics1AV1Txmfp
// CHECK1: SUBSTITUTIONS BEGIN
// CHECK-NEXT1: SUBSTITUTIONS END
// CHECK1: -----
// CHECK1: source.lang.swift.ref.struct ()
// CHECK1: Int
// CHECK1: s:Si
// CHECK1: SUBSTITUTIONS BEGIN
// CHECK-NEXT1: SUBSTITUTIONS END
// CHECK1: -----
// CHECK1: source.lang.swift.ref.generic_type_param (3:14-3:15)
// CHECK1: E
// CHECK1: s:20cursor_info_generics1AV3fooyqd__qd__lF1EL_qd__mfp
// CHECK1: SUBSTITUTIONS BEGIN
// CHECK-NEXT1: SUBSTITUTIONS END
// CHECK1: -----
// CHECK1: SECONDARY SYMBOLS END

struct B<T> {
    struct C<T, E> {
        var t: T
        struct D<F> {
            var e: E
        }
    }
}
var d: B<String>.C<A<Int>, Double>.D<Array<Int>>
let e = d.e

// RUN: %sourcekitd-test -req=cursor -pos=136:11 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: source.lang.swift.ref.var.instance (131:17-131:18)
// CHECK2: e
// CHECK2: s:20cursor_info_generics1BV1CV1DV1eqd_0_vp
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: T -> String (s:20cursor_info_generics1BV1Txmfp -> s:SS)
// CHECK-NEXT2: T -> A<Int> (s:20cursor_info_generics1BV1CV1Tqd__mfp -> s:20cursor_info_generics1AV)
// CHECK-NEXT2: E -> Double (s:20cursor_info_generics1BV1CV1Eqd_0_mfp -> s:Sd)
// CHECK-NEXT2: F -> [Int] (s:20cursor_info_generics1BV1CV1DV1Fqd0__mfp -> s:Sa)
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: SECONDARY SYMBOLS BEGIN
// CHECK2: s:SS
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: -----
// CHECK2: s:20cursor_info_generics1BV1Txmfp
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: -----
// CHECK2: s:20cursor_info_generics1AV
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: -----
// CHECK2: s:20cursor_info_generics1BV1CV1Tqd__mfp
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: -----
// CHECK2: s:Sd
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: -----
// CHECK2: s:20cursor_info_generics1BV1CV1Eqd_0_mfp
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: -----
// CHECK2: s:Sa
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: -----
// CHECK2: s:20cursor_info_generics1BV1CV1DV1Fqd0__mfp
// CHECK2: SUBSTITUTIONS BEGIN
// CHECK-NEXT2: SUBSTITUTIONS END
// CHECK2: -----
// CHECK2: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=135:38 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK3 %s
// CHECK3: source.lang.swift.ref.struct ()
// CHECK3: Array
// CHECK3: s:Sa
// CHECK3: SUBSTITUTIONS BEGIN
// CHECK-NEXT3: Element -> Int (s:Sa7Elementxmfp -> s:Si)
// CHECK-NEXT3: SUBSTITUTIONS END
// CHECK3: SECONDARY SYMBOLS BEGIN
// CHECK3: s:Si
// CHECK3: SUBSTITUTIONS BEGIN
// CHECK-NEXT3: SUBSTITUTIONS END
// CHECK3: -----
// CHECK3: s:Sa7Elementxmfp
// CHECK3: SUBSTITUTIONS BEGIN
// CHECK-NEXT3: SUBSTITUTIONS END
// CHECK3: -----
// CHECK3: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=135:18 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK4 %s
// CHECK4: source.lang.swift.ref.struct (128:12-128:13)
// CHECK4: C
// CHECK4: s:20cursor_info_generics1BV1CV
// CHECK4: SUBSTITUTIONS BEGIN
// CHECK-NEXT4: T -> String (s:20cursor_info_generics1BV1Txmfp -> s:SS)
// CHECK-NEXT4: T -> A<Int> (s:20cursor_info_generics1BV1CV1Tqd__mfp -> s:20cursor_info_generics1AV)
// CHECK-NEXT4: E -> Double (s:20cursor_info_generics1BV1CV1Eqd_0_mfp -> s:Sd)
// CHECK-NEXT4: SUBSTITUTIONS END
// CHECK4: SECONDARY SYMBOLS BEGIN
// CHECK4: s:SS
// CHECK4: SUBSTITUTIONS BEGIN
// CHECK-NEXT4: SUBSTITUTIONS END
// CHECK4: -----
// CHECK4: s:20cursor_info_generics1BV1Txmfp
// CHECK4: SUBSTITUTIONS BEGIN
// CHECK-NEXT4: SUBSTITUTIONS END
// CHECK4: -----
// CHECK4: s:20cursor_info_generics1AV
// CHECK4: SUBSTITUTIONS BEGIN
// CHECK-NEXT4: SUBSTITUTIONS END
// CHECK4: -----
// CHECK4: s:20cursor_info_generics1BV1CV1Tqd__mfp
// CHECK4: SUBSTITUTIONS BEGIN
// CHECK-NEXT4: SUBSTITUTIONS END
// CHECK4: -----
// CHECK4: s:Sd
// CHECK4: SUBSTITUTIONS BEGIN
// CHECK-NEXT4: SUBSTITUTIONS END
// CHECK-NEXT4: -----
// CHECK4: s:20cursor_info_generics1BV1CV1Eqd_0_mfp
// CHECK4: SUBSTITUTIONS BEGIN
// CHECK-NEXT4: SUBSTITUTIONS END
// CHECK4: -----
// CHECK4: SECONDARY SYMBOLS END

var withFuncAndTuple: B<Int>.C<() -> Array<Int>, (Double, Int)>

// RUN: %sourcekitd-test -req=cursor -pos=237:30 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK5 %s
// CHECK5: source.lang.swift.ref.struct (128:12-128:13)
// CHECK5: C
// CHECK5: s:20cursor_info_generics1BV1CV
// CHECK5: SUBSTITUTIONS BEGIN
// CHECK-NEXT5: T -> Int (s:20cursor_info_generics1BV1Txmfp -> s:Si)
// CHECK-NEXT5: T -> () -> Array<Int> (s:20cursor_info_generics1BV1CV1Tqd__mfp -> )
// CHECK-NEXT5: E -> (Double, Int) (s:20cursor_info_generics1BV1CV1Eqd_0_mfp -> )
// CHECK-NEXT5: SUBSTITUTIONS END
// CHECK5: SECONDARY SYMBOLS BEGIN
// CHECK5: s:Si
// CHECK5: SUBSTITUTIONS BEGIN
// CHECK-NEXT5: SUBSTITUTIONS END
// CHECK5: -----
// CHECK5: s:20cursor_info_generics1BV1Txmfp
// CHECK5: SUBSTITUTIONS BEGIN
// CHECK-NEXT5: SUBSTITUTIONS END
// CHECK5: -----
// CHECK5: s:20cursor_info_generics1BV1CV1Tqd__mfp
// CHECK5: SUBSTITUTIONS BEGIN
// CHECK-NEXT5: SUBSTITUTIONS END
// CHECK5: -----
// CHECK5: s:20cursor_info_generics1BV1CV1Eqd_0_mfp
// CHECK5: SUBSTITUTIONS BEGIN
// CHECK-NEXT5: SUBSTITUTIONS END
// CHECK5: -----
// CHECK5: SECONDARY SYMBOLS END

typealias ArrayAlias = Array<Int>
var withTypeAlias = A<ArrayAlias>()

// RUN: %sourcekitd-test -req=cursor -pos=268:21 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK6 %s
// CHECK6: source.lang.swift.ref.struct (1:8-1:9)
// CHECK6: A
// CHECK6: s:20cursor_info_generics1AV
// CHECK6: SUBSTITUTIONS BEGIN
// CHECK-NEXT6: T -> ArrayAlias (s:20cursor_info_generics1AV1Txmfp -> s:20cursor_info_generics10ArrayAliasa)
// CHECK-NEXT6: SUBSTITUTIONS END
// CHECK6: SECONDARY SYMBOLS BEGIN
// CHECK6: s:20cursor_info_generics10ArrayAliasa
// CHECK6: SUBSTITUTIONS BEGIN
// CHECK-NEXT6: SUBSTITUTIONS END
// CHECK6: -----
// CHECK6: s:20cursor_info_generics1AV1Txmfp
// CHECK6: SUBSTITUTIONS BEGIN
// CHECK-NEXT6: SUBSTITUTIONS END
// CHECK6: -----
// CHECK6: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=268:23 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK7 %s
// CHECK7: source.lang.swift.ref.typealias (267:11-267:21)
// CHECK7: ArrayAlias
// CHECK7: s:20cursor_info_generics10ArrayAliasa
// CHECK7: source.lang.swift
// CHECK7: Array<Int>.Type
// CHECK7: $sSaySiGmD
// CHECK7: cursor_info_generics
// CHECK7: <Declaration>typealias ArrayAlias = <Type usr="s:Sa">Array</Type>&lt;<Type usr="s:Si">Int</Type>&gt;</Declaration>
// CHECK7: <decl.typealias><syntaxtype.keyword>typealias</syntaxtype.keyword> <decl.name>ArrayAlias</decl.name> = <ref.struct usr="s:Sa">Array</ref.struct>&lt;<ref.struct usr="s:Si">Int</ref.struct>&gt;</decl.typealias>
// CHECK7: REFERENCED DECLS BEGIN
// CHECK-NEXT7: REFERENCED DECLS END
// CHECK7: SUBSTITUTIONS BEGIN
// CHECK-NEXT7: SUBSTITUTIONS END
// CHECK7: SECONDARY SYMBOLS BEGIN
// CHECK-NEXT7: SECONDARY SYMBOLS END

protocol Prot {
    associatedtype AssocT;
}

struct G<T> : Prot {
    typealias AssocT = Int

    struct H {
        func foo() {
            var c: B<T>.C<AssocT, T>
            var test = c.t
        }
    }
}

// RUN: %sourcekitd-test -req=cursor -pos=314:25 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK8 %s
// CHECK8: source.lang.swift.ref.struct (128:12-128:13)
// CHECK8: C
// CHECK8: s:20cursor_info_generics1BV1CV
// CHECK8: SUBSTITUTIONS BEGIN
// CHECK-NEXT8: T -> T (s:20cursor_info_generics1BV1Txmfp -> s:20cursor_info_generics1GV1Txmfp)
// CHECK-NEXT8: T -> G<T>.AssocT (s:20cursor_info_generics1BV1CV1Tqd__mfp -> s:20cursor_info_generics1GV6AssocTa)
// CHECK-NEXT8: E -> T (s:20cursor_info_generics1BV1CV1Eqd_0_mfp -> s:20cursor_info_generics1GV1Txmfp)
// CHECK-NEXT8: SUBSTITUTIONS END
// CHECK8: SECONDARY SYMBOLS BEGIN
// CHECK8: s:20cursor_info_generics1GV1Txmfp
// CHECK8: SUBSTITUTIONS BEGIN
// CHECK-NEXT8: SUBSTITUTIONS END
// CHECK8: -----
// CHECK8: s:20cursor_info_generics1GV6AssocTa
// CHECK8: SUBSTITUTIONS BEGIN
// CHECK-NEXT8: SUBSTITUTIONS END
// CHECK8: -----
// CHECK8: s:20cursor_info_generics1BV1CV1Tqd__mfp
// CHECK8: SUBSTITUTIONS BEGIN
// CHECK-NEXT8: SUBSTITUTIONS END
// CHECK8: -----
// CHECK8: s:20cursor_info_generics1BV1CV1Eqd_0_mfp
// CHECK8: SUBSTITUTIONS BEGIN
// CHECK-NEXT8: SUBSTITUTIONS END
// CHECK8: -----
// CHECK8: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=315:26 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK9 %s
// CHECK9: source.lang.swift.ref.var.instance (129:13-129:14)
// CHECK9: t
// CHECK9: s:20cursor_info_generics1BV1CV1tqd__vp
// CHECK9: SUBSTITUTIONS BEGIN
// CHECK-NEXT9: T -> T (s:20cursor_info_generics1BV1Txmfp -> s:20cursor_info_generics1GV1Txmfp)
// CHECK-NEXT9: T -> G<T>.AssocT (s:20cursor_info_generics1BV1CV1Tqd__mfp -> s:20cursor_info_generics1GV6AssocTa)
// CHECK-NEXT9: E -> T (s:20cursor_info_generics1BV1CV1Eqd_0_mfp -> s:20cursor_info_generics1GV1Txmfp)
// CHECK-NEXT9: SUBSTITUTIONS END
// CHECK9: SECONDARY SYMBOLS BEGIN
// CHECK9: s:20cursor_info_generics1GV1Txmfp
// CHECK9: SUBSTITUTIONS BEGIN
// CHECK-NEXT9: SUBSTITUTIONS END
// CHECK9: -----
// CHECK9: s:20cursor_info_generics1BV1Txmfp
// CHECK9: SUBSTITUTIONS BEGIN
// CHECK-NEXT9: SUBSTITUTIONS END
// CHECK9: -----
// CHECK9: s:20cursor_info_generics1GV6AssocTa
// CHECK9: SUBSTITUTIONS BEGIN
// CHECK-NEXT9: SUBSTITUTIONS END
// CHECK9: -----
// CHECK9: s:20cursor_info_generics1BV1CV1Tqd__mfp
// CHECK9: SUBSTITUTIONS BEGIN
// CHECK-NEXT9: SUBSTITUTIONS END
// CHECK9: -----
// CHECK9: s:20cursor_info_generics1BV1CV1Eqd_0_mfp
// CHECK9: SUBSTITUTIONS BEGIN
// CHECK-NEXT9: SUBSTITUTIONS END
// CHECK9: -----
// CHECK9: SECONDARY SYMBOLS END

typealias GenericArrayAlias<T> = Array<T>
var IntArray = GenericArrayAlias([1, 2, 3])

// RUN: %sourcekitd-test -req=cursor -pos=381:16 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK10 %s
// CHECK10: source.lang.swift.ref.typealias (380:11-380:28)
// CHECK10: GenericArrayAlias
// CHECK10: s:20cursor_info_generics17GenericArrayAliasa
// CHECK10: SUBSTITUTIONS BEGIN
// CHECK-NEXT10: T -> Int (s:20cursor_info_generics17GenericArrayAliasa1Txmfp -> s:Si)
// CHECK-NEXT10: SUBSTITUTIONS END
// CHECK10: SECONDARY SYMBOLS BEGIN
// CHECK-NEXT10: source.lang.swift.ref.function.constructor ()
// CHECK10: init(_:)
// CHECK10: s:SaySayxGqd__c7ElementQyd__RszSTRd__lufc
// CHECK10: SUBSTITUTIONS BEGIN
// CHECK-NEXT10: Element -> Int (s:Sa7Elementxmfp -> s:Si)
// CHECK-NEXT10: S -> [Int] (s:SaySayxGqd__c7ElementQyd__RszSTRd__lufc1SL_qd__mfp -> s:Sa)
// CHECK-NEXT10: SUBSTITUTIONS END
// CHECK10: -----
// CHECK10: s:Si
// CHECK10: SUBSTITUTIONS BEGIN
// CHECK-NEXT10: SUBSTITUTIONS END
// CHECK10: -----
// CHECK10: s:Sa7Elementxmfp
// CHECK10: SUBSTITUTIONS BEGIN
// CHECK-NEXT10: SUBSTITUTIONS END
// CHECK10: -----
// CHECK10: s:Sa
// CHECK10: SUBSTITUTIONS BEGIN
// CHECK-NEXT10: SUBSTITUTIONS END
// CHECK10: -----
// CHECK10: s:SaySayxGqd__c7ElementQyd__RszSTRd__lufc1SL_qd__mfp
// CHECK10: SUBSTITUTIONS BEGIN
// CHECK-NEXT10: SUBSTITUTIONS END
// CHECK10: -----
// CHECK10: SECONDARY SYMBOLS END

var arr: GenericArrayAlias<Int>

// RUN: %sourcekitd-test -req=cursor -pos=417:10 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK11 %s
// CHECK11: source.lang.swift.ref.typealias (380:11-380:28)
// CHECK11: GenericArrayAlias
// CHECK11: s:20cursor_info_generics17GenericArrayAliasa
// CHECK11: SUBSTITUTIONS BEGIN
// CHECK-NEXT11: T -> Int (s:20cursor_info_generics17GenericArrayAliasa1Txmfp -> s:Si)
// CHECK-NEXT11: SUBSTITUTIONS END
// CHECK11: SECONDARY SYMBOLS BEGIN
// CHECK11: s:Si
// CHECK11: SUBSTITUTIONS BEGIN
// CHECK-NEXT11: SUBSTITUTIONS END
// CHECK11: -----
// CHECK11: s:20cursor_info_generics17GenericArrayAliasa1Txmfp
// CHECK11: SUBSTITUTIONS BEGIN
// CHECK-NEXT11: SUBSTITUTIONS END
// CHECK11: -----
// CHECK11: SECONDARY SYMBOLS END

func fooWithPack<each T: Collection>(_ elem: repeat each T) -> (repeat each T) {
    return (repeat each elem)
}
let res = fooWithPack([1, 2, 3], ["a", "b", "c"])

// RUN: %sourcekitd-test -req=cursor -pos=440:11 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK12 %s
// CHECK12: source.lang.swift.ref.function.free (437:6-437:60)
// CHECK12: fooWithPack(_:)
// CHECK12: s:20cursor_info_generics11fooWithPackyxxQp_txxQpRvzSlRzlF
// CHECK12: SUBSTITUTIONS BEGIN
// CHECK-NEXT12: each T -> Pack{[Int], [String]} (s:20cursor_info_generics11fooWithPackyxxQp_txxQpRvzSlRzlF1TL_xmfp -> )
// CHECK-NEXT12: SUBSTITUTIONS END
// CHECK12: SECONDARY SYMBOLS BEGIN
// CHECK12: s:20cursor_info_generics11fooWithPackyxxQp_txxQpRvzSlRzlF1TL_xmfp
// CHECK12: SUBSTITUTIONS BEGIN
// CHECK-NEXT12: SUBSTITUTIONS END
// CHECK12: -----
// CHECK12: SECONDARY SYMBOLS END

func freeFoo<T>(_ t: T) -> T { return t }
var binExprRes = 1 + freeFoo(42) + 3

// RUN: %sourcekitd-test -req=cursor -pos=457:22 -req-opts=expand_substitutions=true %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %s | %FileCheck -check-prefix=CHECK13 %s
// CHECK13: source.lang.swift.ref.function.free (456:6-456:24)
// CHECK13: freeFoo(_:)
// CHECK13: s:20cursor_info_generics7freeFooyxxlF
// CHECK13: source.lang.swift
// CHECK13: SUBSTITUTIONS BEGIN
// CHECK-NEXT13: T -> Int (s:20cursor_info_generics7freeFooyxxlF1TL_xmfp -> s:Si)
// CHECK-NEXT13: SUBSTITUTIONS END
// CHECK13: SECONDARY SYMBOLS BEGIN
// CHECK13: s:Si
// CHECK13: SUBSTITUTIONS BEGIN
// CHECK-NEXT13: SUBSTITUTIONS END
// CHECK13: -----
// CHECK13: s:20cursor_info_generics7freeFooyxxlF1TL_xmfp
// CHECK13: SUBSTITUTIONS BEGIN
// CHECK-NEXT13: SUBSTITUTIONS END
// CHECK13: -----
// CHECK13: SECONDARY SYMBOLS END

func bar<S>(s: S) {}
func foo<T>(t: T) {
  bar(s: t)
}

var d = B<Int>.C<Int, Int>.D<Int, Int>() // currently does not work

struct Bar<T> {
    init(arg a: T) {}
}
func foo<T>(t: T) {}
foo(t: Bar(arg: 1))
