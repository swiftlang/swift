// RUN: %empty-directory(%t)
// RUN: %validate-incrparse %s --test-case ADD_ELSE

func container() {
#if false
  <<ADD_ELSE<|||#else>>>

