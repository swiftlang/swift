// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ParserASTGen -enable-experimental-com-interop -com-interop-model=microsoft -D EXPECT_MICROSOFT
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ParserASTGen -enable-experimental-com-interop -com-interop-model=corefoundation -D EXPECT_COREFOUNDATION
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ParserASTGen -D EXPECT_DISABLED

// REQUIRES: swift_feature_ParserASTGen

#if EXPECT_MICROSOFT
  #if !$_MicrosoftCOM
    #error("$_MicrosoftCOM should be enabled for the Microsoft COM model")
  #endif
  #if $_CoreFoundationCOM
    #error("$_CoreFoundationCOM should be disabled for the Microsoft COM model")
  #endif
#elseif EXPECT_COREFOUNDATION
  #if $_MicrosoftCOM
    #error("$_MicrosoftCOM should be disabled for the CoreFoundation COM model")
  #endif
  #if !$_CoreFoundationCOM
    #error("$_CoreFoundationCOM should be enabled for the CoreFoundation COM model")
  #endif
#elseif EXPECT_DISABLED
  #if $_MicrosoftCOM
    #error("$_MicrosoftCOM should be disabled when COM interop is disabled")
  #endif
  #if $_CoreFoundationCOM
    #error("$_CoreFoundationCOM should be disabled when COM interop is disabled")
  #endif
#else
  #error("test configuration missing")
#endif
