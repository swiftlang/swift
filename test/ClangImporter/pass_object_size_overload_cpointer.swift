// RUN: %target-swift-frontend -import-objc-header %S/Inputs/pass_object_size.h -primary-file %s -emit-ir -verify | %FileCheck %s --implicit-check-not=_Z6ovl_fpPcU17pass_object_size0

// Forming a C function pointer from a pass_object_size function is possible, but
// only through a thunk that passes a conservative "unknown" size, so the
// conversion is disfavored. Whenever the overload set also has a candidate
// without the attribute, that candidate has to win -- in every syntactic shape
// the conversion can appear in, not just a direct argument position. This is
// also what Clang selects for `size_t (*fp)(char *) = ovl_fp;`.
//
// The --implicit-check-not above is the real assertion: the pass_object_size
// entry point must not be referenced anywhere in the output, which also means no
// thunk was needed.

// REQUIRES: PTRSIZE=64

typealias CFn = @convention(c) (UnsafeMutablePointer<CChar>?) -> Int

func takesCPointer(_ f: CFn) {}
func takesOptional(_ f: CFn?) {}
func takesAutoclosure(_ f: @autoclosure () -> CFn) {}
func takesVariadic(_ f: CFn...) {}
func pick<T>(_ a: T, _ b: T) -> T { return a }
func ident<T>(_ x: T) -> T { return x }
func plainSwift(_ p: UnsafeMutablePointer<CChar>?) -> Int { return 0 }

// CHECK: @_Z6ovl_fpPc

func argumentPosition() { takesCPointer(ovl_fp) }

func parenthesized() { takesCPointer((ovl_fp)) }

func contextualLet() { let f: CFn = ovl_fp; takesCPointer(f) }

func explicitCoercion() { takesCPointer(ovl_fp as CFn) }

func optionalParameter() { takesOptional(ovl_fp) }

func forceUnwrapped() { let f: CFn? = ovl_fp; takesCPointer(f!) }

func nestedOptional() { let f: CFn?? = ovl_fp; _ = f }

func arrayElement() { let a: [CFn] = [ovl_fp]; _ = a }

func dictionaryValue() { let d: [String: CFn] = ["a": ovl_fp]; _ = d }

func tupleElement() { let p: (CFn, Int) = (ovl_fp, 1); _ = p }

func labeledTupleElement() { let p: (fn: CFn, n: Int) = (fn: ovl_fp, n: 1); _ = p }

func ternaryBothBranches(_ c: Bool) { let f: CFn = c ? ovl_fp : ovl_fp; _ = f }

func ternaryAgainstSwiftFunc(_ c: Bool) { let f: CFn = c ? ovl_fp : plainSwift; _ = f }

func returnValue() -> CFn { return ovl_fp }

func closureResult() { let c: () -> CFn = { ovl_fp }; _ = c }

func autoclosureArgument() { takesAutoclosure(ovl_fp) }

func variadicArgument() { takesVariadic(ovl_fp, ovl_fp) }

// The conversion happens through a type variable rather than against a written
// @convention(c) type.
func genericUnification() { let f: CFn = pick(ovl_fp, plainSwift); _ = f }

func genericIdentity() { let f: CFn = ident(ovl_fp); _ = f }

// An already-formed C function pointer being passed along: this is the shape
// where the conversion's source is itself @convention(c).
func cPointerToCPointer() { let g: CFn = ovl_fp; takesCPointer(g) }

struct StoredProperty { let f: CFn = ovl_fp }

let globalVariable: CFn = ovl_fp
