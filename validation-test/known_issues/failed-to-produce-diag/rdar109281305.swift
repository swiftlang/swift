// rdar://109281305
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %t/h.h %t/main.swift

//--- h.h
#pragma clang assume_nonnull begin
void vasm(const float *a, long ia, const float *b, long ib, const float *c,
          float *d, long id, unsigned long n);
#pragma clang assume_nonnull end

//--- main.swift
func f(_ raw: UnsafeMutableRawBufferPointer, _ out: UnsafeMutablePointer<Float>, _ n: Int) {
  vasm(raw.baseAddress!, 2, raw.baseAddress!.advanced(by: 1), 2, [0.5], out, 1, UInt(n)) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
