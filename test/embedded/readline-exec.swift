// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -enable-experimental-feature Embedded -wmo -Xlinker -L%swift_obj_root/lib/swift/embedded/%module-target-triple -Xlinker -lswiftEmbeddedPlatformPOSIX -o %t/a.out

// Build the stdin fixture one line at a time so each case's byte count is
// auditable (a single mega-line of literals silently shifts the `len:` checks).
// lit's built-in shell supports external commands + redirection, but NOT
// command substitution $(...) or brace groups, so the lengths are spelled out.
// RUN: printf 'first\n'                  >  %t/input
// RUN: printf '\n'                       >> %t/input
// RUN: printf 'third\n'                  >> %t/input
// RUN: printf 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n'                   >> %t/input
// RUN: printf 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n'                   >> %t/input
// RUN: printf 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\n'                   >> %t/input
// RUN: printf 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n'                   >> %t/input
// RUN: printf 'has\0nul\n'               >> %t/input
// RUN: printf 'crlf\r\n'                 >> %t/input
// RUN: printf 'embedded\rCR\n'           >> %t/input
// RUN: printf 'no-newline-eof'           >> %t/input
// RUN: %target-run %t/a.out < %t/input | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1 || OS=emscripten
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    // ordinary line
    print("got: \(readLine() ?? "<nil>")")
    // CHECK: got: first

    // empty line -> "" (distinct from EOF nil)
    if let line = readLine() {
      print("empty: \(line.isEmpty)")
    } else {
      print("empty: <nil>")
    }
    // CHECK-NEXT: empty: true

    // ordinary line following the empty one
    print("got: \(readLine() ?? "<nil>")")
    // CHECK-NEXT: got: third

    // Lines of various lengths, including ones longer than getline's initial
    // libc buffer, must round-trip with the exact byte count. Measure
    // `.utf8.count` (bytes), not `.count` (graphemes): grapheme counting would
    // pull in the Unicode grapheme-break tables, which the embedded shim doesn't
    // link, and byte count is the contract here anyway.
    print("len: \(readLine()?.utf8.count ?? -1)")
    // CHECK-NEXT: len: 127
    print("len: \(readLine()?.utf8.count ?? -1)")
    // CHECK-NEXT: len: 128
    print("len: \(readLine()?.utf8.count ?? -1)")
    // CHECK-NEXT: len: 129
    // A long line forces getline to grow its buffer past the initial size.
    print("len: \(readLine()?.utf8.count ?? -1)")
    // CHECK-NEXT: len: 500

    // Embedded NUL byte: the line is length-delimited, NUL is not a terminator.
    // Check both the byte count and that the NUL scalar actually survives into
    // the String (so this proves preservation, not just the return length).
    if let line = readLine() {
      print("nul-len: \(line.utf8.count) nul-kept: \(line.unicodeScalars.contains("\u{0}"))")
    } else {
      print("nul-len: <nil>")
    }
    // CHECK-NEXT: nul-len: 7 nul-kept: true

    // CRLF line ending: getline reads up to and including '\n' (its delimiter)
    // and keeps the '\r' as data; readLine()'s removeNewline() then strips the
    // trailing "\r\n".
    print("got: \(readLine() ?? "<nil>")")
    // CHECK-NEXT: got: crlf

    // A lone '\r' mid-line is data, not a delimiter: getline stops only at the
    // trailing '\n', so the interior CR survives. Assert via byte count + scalar
    // presence (not by emitting the raw CR, which FileCheck handling of carriage
    // returns makes fragile): "embedded\rCR" is 11 bytes with the CR preserved;
    // if CR ended the line it would be 8.
    if let line = readLine() {
      print("cr-len: \(line.utf8.count) cr-kept: \(line.unicodeScalars.contains("\r"))")
    } else {
      print("cr-len: <nil>")
    }
    // CHECK-NEXT: cr-len: 11 cr-kept: true

    // final line with no trailing newline at EOF
    print("got: \(readLine() ?? "<nil>")")
    // CHECK-NEXT: got: no-newline-eof

    // EOF -> nil: a fresh call with no input left has getline return -1, which
    // the caller maps to nil.
    print("got: \(readLine() ?? "<nil>")")
    // CHECK-NEXT: got: <nil>
  }
}
