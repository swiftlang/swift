/* DTrace types and macros for runtime data structures */

enum MetadataKind {
  StructMetadataKind = 1,
  EnumMetadataKind = 2,
  OpaqueMetadataKind = 8,
  TupleMetadataKind = 9,
  FunctionMetadataKind = 10,
  ExistentialMetadataKind = 12,
  MetatypeMetadataKind = 13,
  ObjCClassWrapperKind = 14,
  HeapLocalVariableKind = 64,
  HeapArrayKind = 65
};

typedef struct NominalTypeDescriptor {
  unsigned long kind;
  const char *name;
  unsigned long numFields;
  unsigned long fieldOffsetVectorOffset;
  const char *fieldNames;
} NominalTypeDescriptor;

typedef struct Metadata {
  void *valueWitnessTable;
  unsigned long kind;
} Metadata;

typedef struct ClassMetadata {
  void *valueWitnessTable;
  struct ClassMetadata *isa;
  struct ClassMetadata *super;
  void *data[3];
  NominalTypeDescriptor *descriptor;
  unsigned long instanceSize, instanceAlign;
} ClassMetadata;

#define copyinMetadata(T, ptr) \
  ((T*)copyin((unsigned long)(ptr) - sizeof(void*), sizeof(T)))

#define copyinNominalTypeDescriptor(ptr) \
  ((NominalTypeDescriptor*)copyin((unsigned long)(ptr), sizeof(NominalTypeDescriptor)))
