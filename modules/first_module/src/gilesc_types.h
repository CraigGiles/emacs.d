#if !defined(GILESC_TYPES_H)
#define GILESC_TYPES_H

#if GILESC_DEBUG

#define AssertWithMessage(expression, msg) if(!(expression)) {          \
        printf("===============================================\n");    \
        printf("--              Assertion Failed             --\n");    \
        printf("-----------------------------------------------\n");    \
        printf("File:%s\n", __FILE__);                                  \
        printf("Line:%i\n", __LINE__);                                  \
        printf("Message:\n%s\n", msg);                                  \
        printf("-----------------------------------------------\n");    \
        __builtin_trap();                                               \
    }

#define Assert(expression) AssertWithMessage(expression, "")

#define InvalidCodePath AssertWithMessage(false, "InvalidCodePath")
#define InvalidDefaultCase default: {InvalidCodePath;} break
#else

#define Assert(...)
#define InvalidCodePath
#define InvalidDefaultCase

#endif


#define ArrayCount(ary) (sizeof(ary) / sizeof((ary)[0]))

#define internal static
#define local_persist static
#define global_variable static

#define Kilobytes(value) ((value)*1024LL)
#define Megabytes(value) (Kilobytes(value)*1024LL)
#define Gigabytes(value) (Megabytes(value)*1024LL)
#define Terabytes(value) (Gigabytes(value)*1024LL)

typedef int8_t   s8;
typedef int16_t  s16; // int
typedef int32_t  s32; // double
typedef int64_t  s64; // long
typedef s32      b32; // boolean

typedef uint8_t  u8;
typedef uint16_t u16; // int
typedef uint32_t u32; // double
typedef uint64_t u64; // long

typedef float    f32;
typedef double   f64;

#define U16_MAX  65535
#define S32_MIN  ((s32)0x80000000)
#define S32_MAX  ((s32)0x7fffffff)
#define U32_MIN  0
#define U32_MAX  ((u32)-1)
#define U64_MAX  ((u64)-1)
#define F32_MAX  FLT_MAX
#define F32_MIN  -FLT_MAX

#endif
