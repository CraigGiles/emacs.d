#if !defined(GILESC_STRING_H)
/* ========================================================================
   String

   Version: v1.0.0
   Maintainer: Craig Giles
   License:	This file is placed in the public domain.
   ======================================================================== */
#define GILESC_STRING_H

internal b32 string_begins_with(char *str, char *value);
internal char* eat_spaces(char* str);
internal char* trim(char *str);
internal b32 string_is_empty(char *str);
internal char* eat_string_including(char* str, char* value);
internal u32 string_last_index_of(char* str, char value);
internal char* string_substring(MemoryArena *arena, char* str, u32 start, u32 end = 0);
internal char* string_replace_all(char* str, char from, char to);
internal char* string_prepend(MemoryArena *arena, char* str, char* value);

#if USE_GILESC_STRING
#include <ctype.h>

#include "gilesc_types.h"
#define USE_GILESC_MEMORY 1
#include "gilesc_memory.h"

#include <string.h>

internal b32
string_begins_with(char *str, char *value)
{
    size_t lenpre = strlen(value);
    size_t lenstr = strlen(str);

    b32 result = lenstr < lenpre ?
                          false :
                          strncmp(value, str, lenpre) == 0;

    return result;
}

internal char*
eat_spaces(char* str)
{
    char* result = "";
    if (str) {
        while(isspace((unsigned char)*str)) str++;
        result = str;
    }
    return result;
}

internal char*
trim(char *str)
{
    char *end;

    // Trim leading space
    while(isspace((unsigned char)*str)) {
        str++;
    }

    if (*str == 0) {  // All spaces?
        return str;
    }

    // Trim trailing space
    b32 modified = false;
    end = str + strlen(str) - 1;
    while(end > str &&
          isspace((unsigned char)*end))
    {
        modified = true;
        end--;
    }

    // Write new null terminator character
    if (modified) {
        end[1] = '\0';
    }

    return str;
    #if 0
    char* result = eat_spaces(str);

    size_t length = strlen(result);
    char c = result[length-1];

    while (char c == ' ') {
        result--;
        c = result[length-1];
    }
    #endif
}

internal b32
string_is_empty(char *str)
{
    b32 result = false;
    if (strcmp(str, "") == 0)
    {
        result = true;
    } else {
        result = (strlen(trim(str)) == 0);
    }
    return result;
}

// from the beginning of the string, remove all contents up to and
// including the 'value' If the string is "Hello! World!" and you call
// eat_string_including(str, "!") then the result of the function will
// be " World!"
internal char*
eat_string_including(char* str, char* value)
{
    while (strcmp(value, "") != 0) {
        if (string_begins_with(str, value)) value++;
        str++;
    }

    return str;
}

internal u32
string_last_index_of(char* str, char value)
{
    u32 result = 1;

    u32 string_length = strlen(str) - 1;
    for (u32 index = string_length;
         index > 0;
         --index)
    {
        if (str[index] == value)
        {
            result = index;
            break;
        }
    }

    return result;
}

internal char*
string_substring(MemoryArena *arena, char* str, u32 start, u32 end = 0)
{
    if (end == 0)
    {
        end = strlen(str);
    }
    
    u32 length = end - start;

    char* result = ReserveMemoryForString(arena, length);
    char* at = str + start;
    stpncpy(result, at, length);
    return result;
}

internal char*
string_replace_all(char* str, char from, char to)
{
    char* at = str;

    while (!string_is_empty(at))
    {
        if (*at == from)
        {
            *at = to;
        }
            
        ++at;
    }

    return str;
}

internal char*
string_prepend(MemoryArena *arena, char* str, char* value)
{
    u32 length = strlen(str) + strlen(value);
    char* result = ReserveMemoryForString(arena, length);
    sprintf(result, "%s%s", value, str);
    result = string_substring(arena, result, 0, length); // TODO piggy

    return result;
}
#endif
#endif
