// The swift_name rename to `deinit` is invalid, so it is ignored (with a
// warning) and the function is imported under its original C name, which the
// `destroy:` attribute resolves to.

typedef struct __attribute__((swift_attr("~Copyable"))) __attribute__((swift_attr("destroy:renamedDestroyType_free"))) RenamedDestroyType {
  void *storage;
} RenamedDestroyType;

// expected-warning@+1{{custom Swift name 'RenamedDestroyType.deinit(self:)' ignored because it is not valid for global function; imported as 'renamedDestroyType_free' instead}}
void renamedDestroyType_free(RenamedDestroyType x) __attribute__((swift_name("RenamedDestroyType.deinit(self:)")));
