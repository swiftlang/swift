struct S1 {
  struct S2 {
    bool A : 1;
  };
};

struct S3 {
  struct S4 { };
};

union U1 {
    union U2 {};
};

union U3 {
    enum E1 {};
};

union U4 {
    struct S5 {};
};

struct S6 {
    enum E3 {};
};

struct S7 {
  union U5 {
      union U6 {};
  };
};

struct S8 {
  struct S9 {
      union U7 {};
  };
};

struct S10 {
  union U8 {
      enum E4 {};
  };
};

// TODO: Nested class templates (SR-13853).
