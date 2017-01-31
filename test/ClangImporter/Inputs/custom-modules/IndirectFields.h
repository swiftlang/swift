struct StructWithIndirectField {
    union {
        int a;
        int b;
    };
    int c;
    int d : 3; /* Imported as a computed property */
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
