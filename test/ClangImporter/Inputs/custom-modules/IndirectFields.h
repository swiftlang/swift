struct StructWithIndirectField {
    union {
        int a;
        int b;
    };
    int c;
    int d : 3; /* Imported as a computed property */
};

struct StructWithIndirectField2 {
  union {
    unsigned v;
    struct {
      unsigned x : 8;
      unsigned y : 8;
    };
  };
};

struct StructWithIndirectField2Copy {
  union {
    unsigned v;
    struct {
      unsigned x : 16;
      unsigned y : 16;
    };
  };
};

union UnionWithIndirectField {
    struct {
        int a;
        int b;
    };
    int c;
};

struct DeepIndirectField {
    union {
        struct {
            int a;
            int b;
        };
        struct {
            int c;
            int d;
        };
    };
};
