struct ImmortalFRT {
private:
  int priv = 1;
  
public:
  int publ = 2;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

struct FRTCustomStringConvertible {
public:
private:
  int priv = 1;

public:
  int publ = 2;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

struct FRType {
private:
  int priv = 1;

public:
  int publ = 2;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain")))
__attribute__((swift_attr("release:release")));

void retain(FRType *v) {};
void release(FRType *v) {};

