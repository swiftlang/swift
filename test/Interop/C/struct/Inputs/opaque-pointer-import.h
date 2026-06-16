// Two structs with full definitions (complete types).
// OpaqueHandleImpl is annotated to have its pointer imported as OpaquePointer;
// PlainStruct is not annotated, so its pointer imports as UnsafeMutablePointer<PlainStruct>.

struct __attribute__((swift_attr("import_opaque_pointer"))) OpaqueHandleImpl {
  int value;
};

struct PlainStruct {
  int value;
};

typedef struct OpaqueHandleImpl *OpaqueHandle;

OpaqueHandle createOpaqueHandle(void);
void consumeOpaqueHandle(OpaqueHandle h);
struct OpaqueHandleImpl *getOpaqueHandleRaw(void);

struct PlainStruct *createPlainStruct(void);
void consumePlainStruct(struct PlainStruct *s);
