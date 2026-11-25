struct V { int v; };
struct B : virtual V {};
struct C : B { int c; };

struct DV { int v; virtual void f() const {} };
struct L : virtual DV { int l; };
struct R : virtual DV { int r; };
struct D : L, R {};
