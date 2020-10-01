// RUN: %scale-test --sum-multi --begin 5 --end 16 --step 5 --select NumIterableDeclContextParsed -Xfrontend=-enable-objc-interop -Xfrontend=-disable-objc-attr-requires-foundation-module %s
// REQUIRES: asserts

// Dynamic member lookup should not force delayed parsing of structs, enums or protocol
// extensions.
struct S${N} {}
enum E${N} {}
extension Sequence {}

% if int(N) == 1:
class C {
    @objc func isObjCMember() {}
}
% end

func f${N}(a: AnyObject) {
  a.isObjCMember!()
}
