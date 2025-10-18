#include "rdar81617749.h"

#pragma clang assume_nonnull begin

@implementation PFXObject
- (void)performSingleFlaggy1WithCompletionHandler:
    (void (^)(BOOL, CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 1))) {
  completionHandler(YES, ^{
    fprintf(stdout, "%s\n", "performSingleFlaggy1");
  });
}
- (void)performSingleFlaggy2WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 2))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performSingleFlaggy2");
      },
      YES);
}

- (void)performSingleErrory1WithCompletionHandler:
    (void (^)(NSError *_Nullable,
              CompletionHandler _Nullable))completionHandler {
  completionHandler(NULL, ^{
    fprintf(stdout, "%s\n", "performSingleErrory1");
  });
}
- (void)performSingleErrory2WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable,
              NSError *_Nullable))completionHandler {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performSingleErrory2");
      },
      NULL);
}

- (void)performSingleBothy12WithCompletionHandler:
    (void (^)(NSError *_Nullable, BOOL,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 2))) {
  completionHandler(NULL, YES, ^{
    fprintf(stdout, "%s\n", "performSingleBothy12");
  });
}
- (void)performSingleBothy13WithCompletionHandler:
    (void (^)(NSError *_Nullable, CompletionHandler _Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  completionHandler(
      NULL,
      ^{
        fprintf(stdout, "%s\n", "performSingleBothy13");
      },
      YES);
}
- (void)performSingleBothy21WithCompletionHandler:
    (void (^)(BOOL, NSError *_Nullable,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 1))) {
  completionHandler(YES, NULL, ^{
    fprintf(stdout, "%s\n", "performSingleBothy21");
  });
}
- (void)performSingleBothy23WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, NSError *_Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performSingleBothy23");
      },
      NULL, YES);
}
- (void)performSingleBothy31WithCompletionHandler:
    (void (^)(BOOL, CompletionHandler _Nullable,
              NSError *_Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 1))) {
  completionHandler(
      YES,
      ^{
        fprintf(stdout, "%s\n", "performSingleBothy31");
      },
      NULL);
}
- (void)performSingleBothy32WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, BOOL,
              NSError *_Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 2))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performSingleBothy32");
      },
      YES, NULL);
}

- (void)performDoubleFlaggy1WithCompletionHandler:
    (void (^)(BOOL, CompletionHandler _Nullable,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 1))) {
  completionHandler(
      YES,
      ^{
        fprintf(stdout, "%s\n", "performDoubleFlaggy1, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleFlaggy1, part 2");
      });
}
- (void)performDoubleFlaggy2WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, BOOL,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 2))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleFlaggy2, part 1");
      },
      YES,
      ^{
        fprintf(stdout, "%s\n", "performDoubleFlaggy2, part 2");
      });
}
- (void)performDoubleFlaggy3WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, CompletionHandler _Nullable,
              BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleFlaggy3, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleFlaggy3, part 2");
      },
      YES);
}

- (void)performDoubleErrory1WithCompletionHandler:
    (void (^)(NSError *_Nullable, CompletionHandler _Nullable,
              CompletionHandler _Nullable))completionHandler {
  completionHandler(
      NULL,
      ^{
        fprintf(stdout, "%s\n", "performDoubleErrory1, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleErrory1, part 2");
      });
}
- (void)performDoubleErrory2WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, NSError *_Nullable,
              CompletionHandler _Nullable))completionHandler {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleErrory2, part 1");
      },
      NULL,
      ^{
        fprintf(stdout, "%s\n", "performDoubleErrory2, part 2");
      });
}
- (void)performDoubleErrory3WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, CompletionHandler _Nullable,
              NSError *_Nullable))completionHandler {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleErrory3, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleErrory3, part 2");
      },
      NULL);
}

- (void)performDoubleBothy12WithCompletionHandler:
    (void (^)(NSError *_Nullable, BOOL, CompletionHandler _Nullable,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 2))) {
  completionHandler(
      NULL, YES,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy12, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy12, part 2");
      });
}
- (void)performDoubleBothy13WithCompletionHandler:
    (void (^)(NSError *_Nullable, CompletionHandler _Nullable, BOOL,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  completionHandler(
      NULL,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy13, part 1");
      },
      YES,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy13, part 2");
      });
}
- (void)performDoubleBothy14WithCompletionHandler:
    (void (^)(NSError *_Nullable, CompletionHandler _Nullable,
              CompletionHandler _Nullable, BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 4))) {
  completionHandler(
      NULL,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy14, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy14, part 2");
      },
      YES);
}

- (void)performDoubleBothy21WithCompletionHandler:
    (void (^)(BOOL, NSError *_Nullable, CompletionHandler _Nullable,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 1))) {
  completionHandler(
      YES, NULL,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy21, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy21, part 2");
      });
}
- (void)performDoubleBothy23WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, NSError *_Nullable, BOOL,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy23, part 1");
      },
      NULL, YES,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy23, part 2");
      });
}
- (void)performDoubleBothy24WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, NSError *_Nullable,
              CompletionHandler _Nullable, BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 4))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy24, part 1");
      },
      NULL,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy24, part 2");
      },
      YES);
}

- (void)performDoubleBothy31WithCompletionHandler:
    (void (^)(BOOL, CompletionHandler _Nullable, NSError *_Nullable,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 1))) {
  completionHandler(
      YES,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy31, part 1");
      },
      NULL,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy31, part 2");
      });
}
- (void)performDoubleBothy32WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, BOOL, NSError *_Nullable,
              CompletionHandler _Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 2))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy32, part 1");
      },
      YES, NULL,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy32, part 2");
      });
}
- (void)performDoubleBothy34WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, CompletionHandler _Nullable,
              NSError *_Nullable, BOOL))completionHandler
    __attribute__((swift_async_error(zero_argument, 4))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy34, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy34, part 2");
      },
      NULL, YES);
}

- (void)performDoubleBothy41WithCompletionHandler:
    (void (^)(BOOL, CompletionHandler _Nullable, CompletionHandler _Nullable,
              NSError *_Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 1))) {
  completionHandler(
      YES,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy41, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy41, part 2");
      },
      NULL);
}
- (void)performDoubleBothy42WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, BOOL, CompletionHandler _Nullable,
              NSError *_Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 2))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy42, part 1");
      },
      YES,
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy42, part 2");
      },
      NULL);
}
- (void)performDoubleBothy43WithCompletionHandler:
    (void (^)(CompletionHandler _Nullable, CompletionHandler _Nullable, BOOL,
              NSError *_Nullable))completionHandler
    __attribute__((swift_async_error(zero_argument, 3))) {
  completionHandler(
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy43, part 1");
      },
      ^{
        fprintf(stdout, "%s\n", "performDoubleBothy43, part 2");
      },
      YES, NULL);
}
@end

#pragma clang assume_nonnull end
