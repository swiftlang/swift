

typedef struct HeapObject {
  uintptr_t M; /* Metadata */
} HeapObject;

typedef struct NominalTypeDescriptor {
  uintptr_t Zero;
  uintptr_t Name;
} NominalTypeDescriptor;

typedef struct ClassMetadata {
  uintptr_t Kind;
  struct ClassMetadata *super;
  void *data[2];
  uintptr_t data2;
  uint32_t InstanceAddressPoint;
  uint32_t InstanceSize;
  uint16_t InstanceAlignMask;
  uint16_t Reserved;
  uint32_t ClassSize;
  uint32_t ClassAddressPoint;
  NominalTypeDescriptor *Descriptor;
} ClassMetadata;

pid$target:libswiftCore:swift_retain:entry / arg0 != 0 /
{
    this->ptr = (HeapObject *)copyin(arg0, sizeof(HeapObject));
    this->metadata = (ClassMetadata *)copyin(this->ptr->M, sizeof(ClassMetadata));
    this->descriptor = (NominalTypeDescriptor *)copyin((uintptr_t)this->metadata->Descriptor, sizeof(NominalTypeDescriptor));
    this->clsname = copyinstr(this->descriptor->Name);
    @type_count["swift_retain", this->clsname] = count();
    @counts["swift_retain", "global"] = count();
}

pid$target:libswiftCore:swift_release:entry / arg0 != 0 /
{
    this->ptr = (HeapObject *)copyin(arg0, sizeof(HeapObject));
    this->metadata = (ClassMetadata *)copyin(this->ptr->M, sizeof(ClassMetadata));
    this->descriptor = (NominalTypeDescriptor *)copyin((uintptr_t)this->metadata->Descriptor, sizeof(NominalTypeDescriptor));
    this->clsname = copyinstr(this->descriptor->Name);
    @type_count["swift_release", this->clsname] = count();
    @counts["swift_release", "global"] = count();
}

pid$target:libobjc:objc_retain:entry
{
    @counts["objc_retain", "global"] = count();
}

pid$target:libobjc:objc_release:entry
{
    @counts["objc_release", "global"] = count();
}
