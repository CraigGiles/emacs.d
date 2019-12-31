#if !defined(GILESC_MEMORY_H)
/* ========================================================================
   Memory

   Version: v1.0.0
   Maintainer: Craig Giles
   License:	This file is placed in the public domain.
   ======================================================================== */
#define GILESC_MEMORY_H

#if USE_GILESC_MEMORY

#define Kilobytes(value) ((value)*1024LL)
#define Megabytes(value) (Kilobytes(value)*1024LL)
#define Gigabytes(value) (Megabytes(value)*1024LL)
#define Terabytes(value) (Gigabytes(value)*1024LL)

// NOTE(craig): https://github.com/mtrebi/memory-allocators
struct MemoryArena {
    size_t size;
    u8 *base;
    size_t used;

    s32 temp_count;
};

struct TemporaryMemory {
    MemoryArena *arena;
    size_t used;
};

struct TaskWithMemory
{
    b32 BeingUsed;
    MemoryArena Arena;

    TemporaryMemory MemoryFlush;
};

struct TaskList {
    s32 count;
    TaskWithMemory tasks[4];
};

#define ReserveMemoryForStruct(Arena, type) (type *)reserve_memory_(Arena, sizeof(type))
#define ReserveMemoryForArray(Arena, Count, type) (type *)reserve_memory_(Arena, (Count)*sizeof(type))
#define ReserveMemoryForSize(Arena, Size) reserve_memory_(Arena, Size)
#define ReserveMemoryForString(Arena, Length) (char*)reserve_memory_(Arena, sizeof(char) * Length)

internal void *
reserve_memory_(MemoryArena *arena, size_t size, size_t alignment = 4)
{
    Assert((arena->used + size) <= arena->size);
    void *result = arena->base + arena->used;
    arena->used += size;

    return result;
}

internal void
init_memory_arena(MemoryArena *arena, size_t size, void *base)
{
    arena->size = size;
    arena->base = (u8*)base;
    arena->used = 0;
}

inline TemporaryMemory
begin_temporary_memory(MemoryArena *arena) {
    TemporaryMemory result;

    result.arena = arena;
    result.used = arena->used;

    ++arena->temp_count;

    return(result);
}

inline void
end_temporary_memory(TemporaryMemory TempMem) {
    MemoryArena *arena = TempMem.arena;
    Assert(arena->used >= TempMem.used);
    arena->used = TempMem.used;
    Assert(arena->temp_count > 0);
    --arena->temp_count;
}

inline void
check_arena(MemoryArena *arena) {
    Assert(arena->temp_count == 0);
}

inline void
sub_arena(MemoryArena *destination, MemoryArena *source, size_t size, size_t alignment = 16)
{
    destination->size = size;
    destination->base = (u8 *)reserve_memory_(source, size, alignment);
    destination->used = 0;
    destination->temp_count = 0;
}

inline f32
bytes_to_kilobytes(u32 bytes)
{
    f32 result = (f32)(bytes * 0.001);
    return result;
}

inline f32
bytes_to_megabytes(u32 bytes)
{
    f32 result = (f32)(bytes * 0.000001);
    return result;
}

#endif 

#endif
