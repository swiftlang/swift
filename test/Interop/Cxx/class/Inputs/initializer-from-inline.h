inline int get42() { return 42; }
struct Hold42 { int m = get42(); };

template <typename T> T passThroughArgT(T t) { return t; }
struct Hold23 { int m = passThroughArgT<int>(23); };

struct HoldMemberThatHolds42 { Hold42 m; };
struct HoldMemberThatHoldsMemberThatHolds42 { HoldMemberThatHolds42 m; };

inline int get42Level4() { return get42(); }
inline int get42Level3() { return get42Level4(); }
inline int get42Level2() { return get42Level3(); }
inline int get42Level1() { return get42Level2(); }
struct Hold42WithLongInitCallGraph { int m = get42Level1(); };
