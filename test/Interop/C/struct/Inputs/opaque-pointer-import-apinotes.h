// Struct with a complete definition but no swift_attr in the header.
// The OpaquePointerImportAPINotes.apinotes file applies the annotation.

struct FileImpl {
  int fd;
  char buf[64];
};

typedef struct FileImpl *FileHandle;

FileHandle openFileHandle(void);
void closeFileHandle(FileHandle f);
struct FileImpl *getRawFileImpl(void);
