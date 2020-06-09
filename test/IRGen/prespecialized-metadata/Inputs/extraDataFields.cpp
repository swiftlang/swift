#include "extraDataFields.h"
#include <swift/Runtime/Metadata.h>

using namespace swift;

namespace llvm {
  int DisableABIBreakingChecks;
  int EnableABIBreakingChecks;
}

void *completeMetadata(void *uncastMetadata) {
  auto *metadata = static_cast<Metadata *>(uncastMetadata);

  auto request =
        MetadataRequest(MetadataState::Complete, /*isNonBlocking*/ false);
  auto response = swift_checkMetadataState(request, metadata);
  return (void *)response.Value;
}

int64_t *trailingFlagsForStructMetadata(void *uncastMetadata) {
  auto *metadata = static_cast<StructMetadata *>(uncastMetadata);
  auto *description = metadata->getDescription();
  if (!description->isGeneric())
    return nullptr;

  auto *trailingFlags = metadata->getTrailingFlags();
  if (trailingFlags == nullptr)
    return nullptr;

  auto *retval = (int64_t *)malloc(sizeof(int64_t));
  *retval = trailingFlags->getOpaqueValue();
  return retval;
}

const uint32_t *fieldOffsetsForStructMetadata(void *uncastMetadata) {
  auto *metadata = static_cast<StructMetadata *>(uncastMetadata);
  return metadata->getFieldOffsets();
}

int64_t *trailingFlagsForEnumMetadata(void *uncastMetadata) {
  auto *metadata = static_cast<EnumMetadata *>(uncastMetadata);
  auto *description = metadata->getDescription();
  if (!description->isGeneric())
    return nullptr;

  auto *trailingFlags = metadata->getTrailingFlags();
  if (trailingFlags == nullptr)
    return nullptr;

  auto *retval = (int64_t *)malloc(sizeof(int64_t));
  *retval = trailingFlags->getOpaqueValue();
  return retval;
}

size_t *payloadSizeForEnumMetadata(void *uncastMetadata) {
  auto *metadata = static_cast<EnumMetadata *>(uncastMetadata);
  if (!metadata->hasPayloadSize())
    return nullptr;

  auto *retval = (size_t *)malloc(sizeof(size_t));
  *retval = metadata->getPayloadSize();
  return retval;
}

