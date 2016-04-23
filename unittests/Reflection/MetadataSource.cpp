#include "swift/Reflection/ReflectionContext.h"
#include "swift/Reflection/MetadataSource.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace reflection;

TEST(Reflection, MetadataSourceEncodingRoundTrip) {
  MetadataSourceBuilder Builder;

  auto CB = Builder.createClosureBinding(123);
  CB->dump();
  auto CB_encoded = CB->encode();
  std::cerr << "Encoded as: " << CB_encoded << std::endl;
  auto CB_MS_decoded = MetadataSource::decode(Builder, CB_encoded);
  CB_MS_decoded->dump();
  auto CB_decoded = cast<ClosureBindingMetadataSource>(CB_MS_decoded);

  EXPECT_EQ(CB->getIndex(), CB_decoded->getIndex());

  std::cerr << std::endl;

  auto RC = Builder.createReferenceCapture(123);
  RC->dump();
  auto RC_encoded = RC->encode();
  std::cerr << "Encoded as: " <<  RC_encoded << std::endl;
  auto RC_MS_decoded = MetadataSource::decode(Builder, RC_encoded);
  RC_MS_decoded->dump();
  auto RC_decoded = cast<ReferenceCaptureMetadataSource>(RC_MS_decoded);

  EXPECT_EQ(RC->getIndex(), RC_decoded->getIndex());

  std::cerr << std::endl;

  auto GA_CB = Builder.createGenericArgument(0, CB);
  GA_CB->dump();
  auto GA_GA_CB = Builder.createGenericArgument(1, GA_CB);
  auto GA_encoded = GA_GA_CB->encode();
  std::cerr << "Encoded as: " << GA_encoded << std::endl;
  auto GA_MS_decoded = MetadataSource::decode(Builder, GA_encoded);
  GA_MS_decoded->dump();
  auto GA_decoded = cast<GenericArgumentMetadataSource>(GA_MS_decoded);

  EXPECT_EQ(GA_GA_CB->getIndex(), GA_decoded->getIndex());

  std::cerr << std::endl;

  auto GA_GA_decoded = cast<GenericArgumentMetadataSource>(GA_decoded->getSource());

  EXPECT_EQ(GA_CB->getIndex(), GA_GA_decoded->getIndex());

  std::cerr << std::endl;
}
