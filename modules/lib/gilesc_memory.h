#if !defined(GILESC_MEMORY_H)
/* ========================================================================
   $File: $
   $Date: $
   $Revision: $
   $Creator: Craig Giles $
   $Notice: (C) Copyright 2019 by Craig Giles. All Rights Reserved. $
   ======================================================================== */

#define GILESC_MEMORY_H

#define MEMORY_ARENA_BLOCK_SIZE 1024

struct MemoryArena {
    size_t size;
    u8 *base;
    size_t used;
};

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
initialize_memory_arena_(MemoryArena *arena, size_t size, void *base)
{
    arena->size = size;
    arena->base = (u8*)base;
    arena->used = 0;
}

#define STACK_MEMORY_ARENA(name) { \
    u8 block[MEMORY_ARENA_BLOCK_SIZE] = {}; \
    initialize_memory_arena_(name, MEMORY_ARENA_BLOCK_SIZE, &block); \
}\

#endif
