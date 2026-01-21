#pragma once

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
// expected-note@+1 {{annotate 'init()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
FRTImplicitDefaultCtor1 {
  mutable int refs = 1;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
// expected-note@+1 {{annotate 'init()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
FRTImplicitDefaultCtor0 {
  mutable int refs = 0;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
FRTExplicitDefaultCtorNoAnnotation {
  mutable int refs = 1;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }

  // expected-note@+1 {{annotate 'init()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
  FRTExplicitDefaultCtorNoAnnotation() = default;
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
FRTExplicitDefaultCtor1 {
  mutable int refs = 1;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }

  __attribute__((swift_attr("returns_retained")))
  FRTExplicitDefaultCtor1() = default;
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
FRTExplicitDefaultCtor0 {
  mutable int refs = 0;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }


  __attribute__((swift_attr("returns_unretained")))
  FRTExplicitDefaultCtor0() = default;
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
FRTUserDefaultCtorNoAnnotation {
  mutable int refs;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }

  // expected-note@+1 {{annotate 'init()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
  FRTUserDefaultCtorNoAnnotation() : refs(0) {}
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
FRTUserDefaultCtor1 {
  mutable int refs;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }

  __attribute__((swift_attr("returns_retained")))
  FRTUserDefaultCtor1() : refs(1) {}
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
FRTUserDefaultCtor0 {
  mutable int refs;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }

  __attribute__((swift_attr("returns_unretained")))
  FRTUserDefaultCtor0() : refs(0) {}
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
FRTMixedConventionCtors {
  mutable int refs;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }

  __attribute__((swift_attr("returns_unretained")))
  FRTMixedConventionCtors() : refs(0) {}

  __attribute__((swift_attr("returns_retained")))
  FRTMixedConventionCtors(int) : refs(1) {}

  // expected-note@+1 {{annotate 'init(_:_:)' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
  FRTMixedConventionCtors(int, int) : refs(0) {}
};

struct
  __attribute__((swift_attr("import_reference")))
  __attribute__((swift_attr("retain:.retain")))
  __attribute__((swift_attr("release:.release")))
  __attribute__((swift_attr("returned_as_unretained_by_default")))
FRTMixedConventionCtorsUnretainedByDefault {
  mutable int refs;
  void retain() const { check(); ++refs; }
  void release() const { --refs; check(); if (refs == 0) delete this; }
  void check() const { if (refs < 0) __builtin_trap(); }

  __attribute__((swift_attr("returns_retained")))
  FRTMixedConventionCtorsUnretainedByDefault() : refs(1) {}

  __attribute__((swift_attr("returns_unretained")))
  FRTMixedConventionCtorsUnretainedByDefault(int) : refs(0) {}

  FRTMixedConventionCtorsUnretainedByDefault(int, int) : refs(0) {}
};
